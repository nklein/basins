;;; balls.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Balls
;;;; ------------------------------------------------------------------

(defun random-ball-color ()
  (let ((a (v! 1.0 1.0 0.4))
        (b (v! 0.9 0.5 0.3))
        (c (v! 0.95 0.85 1.0)))
    (v3:lerp a
             (v3:lerp b c (random 1.0))
             (random 1.0))))

(defclass ball ()
  ((position :accessor pos :initarg :position)
   (velocity :accessor vel :initarg :velocity)
   (radius :reader radius :initarg :radius)
   (color :reader color :initarg :color)
   (bx :accessor bx :initform -1)
   (by :accessor by :initform -1))
  (:default-initargs :position (v! 0 0 0)
                     :velocity (v! 0 0 0)
                     :radius *ball-radius*
                     :color (random-ball-color)))

(defun scale-ball-velocity (b s)
  (setf (vel b) (v3:*s (vel b) s)))

(defun scale-all-ball-velocities (s)
  (dolist (b *balls*)
    (scale-ball-velocity b s)))

(defun random-velocity ()
  (let ((cis-theta (* (random 0.00001)
                      (cis (random (* 2 pi))))))
    (v! (cospart cis-theta)
        (sinpart cis-theta)
        0)))

(defun reassign-random-velocities ()
  (dolist (b *balls*)
    (setf (vel b) (random-velocity))))

(defun random-xy ()
  (flet ((rnd ()
           (* *grid-scale* (- (random 1.0) 0.5))))
    (list (rnd) (rnd))))

(defun make-initial-balls (&optional (n 250))
  "Return the list of BALLs the simulation starts with."
  (loop :repeat n
        :for (x y) := (random-xy)
        :collecting (make-instance 'ball
                                   :position (v! x y (+ (influence-at x y) *ball-radius*))
                                   :velocity (random-velocity)
                                   :radius *ball-radius*)))

(defun add-balls (n)
  (setf *balls* (nconc (make-initial-balls n)
                       *balls*))
  (values))

(defun snap-to-surface (b)
  "Put B back on the heightfield.

Z is a dependent coordinate: the dynamics live entirely in the XY plane, so
this just re-derives Z from X and Y."
  (with-accessors ((pos pos)) b
    (setf (v:z pos) (+ (influence-at (v:x pos) (v:y pos))
                       (radius b)))))

(defun shift-ball (b dx dy)
  "Shift B by (DX, DY) in the plane, keeping its total energy fixed.

Nudging a ball sideways changes the height of the field under it, and so its
potential energy.  Left uncompensated that is a pump: down in a basin, gravity
turns potential into kinetic every step as a contact compresses, and pushing
the overlap back out restores the potential while the kinetic stays.  Taking
the change out of the kinetic energy closes the loop."
  (with-accessors ((pos pos) (vel vel) (radius radius)) b
    (let ((h0 (- (v:z pos) radius)))
      (incf (v:x pos) dx)
      (incf (v:y pos) dy)
      (let ((h1 (influence-at (v:x pos) (v:y pos))))
        (setf (v:z pos) (+ h1 (radius b)))
        ;; U = -g * h, so the move costs (-g) * (h1 - h0).
        (let* ((du (* (- *gravitational-force*) (- h1 h0)))
               (ke (kinetic-energy b))
               (ke* (- ke du)))
          (cond ((not (plusp ke)) nil)   ; nothing to scale
                ((plusp ke*)
                 (setf vel (v3:*s vel (sqrt (/ ke* ke)))))
                ;; Not enough kinetic energy to pay for the lift.  Stop the
                ;; ball rather than take the square root of a negative.
                (t
                 (setf vel (v! 0 0 0)))))))))

(defun update-ball (b dt)
  (let ((gforce *gravitational-force*)
        (limit (- (/ *grid-scale* 2) *epsilon*))
        (dt/2 (/ dt 2.0)))
    (with-accessors ((pos pos) (vel vel)) b
      (flet ((kick (h)
               ;; The surface enters only through its gradient: a = g * grad h,
               ;; which is -grad U for U = -g * h, so this is a particle in a
               ;; potential well.
               (let ((gradient (dinfluence-at (v:x pos) (v:y pos))))
                 (setf vel (v3:+ vel
                                 (v! (* gforce h (v:x gradient))
                                     (* gforce h (v:y gradient))
                                     0))))))
        ;; Velocity Verlet: half kick, full drift, half kick.  Unlike the plain
        ;; symplectic Euler this replaces, it is time-reversible, and that is
        ;; the property that keeps the energy error a bounded wobble instead of
        ;; a slow one-way slide.
        (kick dt/2)
        (setf pos (v3:+ pos
                        (v3:*s vel dt)))
        ;; Bounce at the edges.  Mirror the overshoot rather than clamping it
        ;; away: clamping teleports the ball to the wall, and on a slope that
        ;; jump in height is free potential energy.  A mirror is its own
        ;; inverse, so it leaves the reversibility above intact.
        (macrolet ((bounce (axis)
                     `(let ((p (,axis pos)))
                        (cond ((> p limit)
                               (setf (,axis pos) (- (* 2 limit) p)
                                     (,axis vel) (- (,axis vel))))
                              ((< p (- limit))
                               (setf (,axis pos) (- (* -2 limit) p)
                                     (,axis vel) (- (,axis vel))))))))
          (bounce v:x)
          (bounce v:y))
        (kick dt/2))
      (snap-to-surface b))))

(defun put-ball-in-bucket (buckets bucket-grid-coord-fn b &optional (force nil))
  (let ((ox (bx b))
        (oy (by b))
        (pos (pos b)))
    (let ((x (funcall bucket-grid-coord-fn (v:x pos)))
          (y (funcall bucket-grid-coord-fn (v:y pos))))
      (when (or force
                (/= ox x)
                (/= oy y))
        (when (and (plusp ox)
                   (plusp oy))
          (setf (aref buckets ox oy) (remove b (aref buckets ox oy))))
        (push b (aref buckets x y))
        (setf (bx b) x
              (by b) y)))))

(defun reconcile-collisions (a rest buckets bucket-grid-coord-fn)
  ;; Since all balls are the same mass and perfectly elastic, a pair just
  ;; exchanges the component of relative velocity along the line of centres.
  ;;
  ;; All of it happens in XY.  The dynamics live in that plane, and letting Z
  ;; into the impulse leaves a phantom VEL.Z that never decays and never does
  ;; anything -- Z position is re-derived from the surface either way -- but
  ;; which every energy tally then counts.
  (with-accessors ((pa pos) (va vel)) a
    (let ((ra (radius a)))
      (dolist (b rest)
        (with-accessors ((pb pos) (vb vel)) b
          (let* ((dx (- (v:x pb) (v:x pa)))
                 (dy (- (v:y pb) (v:y pa)))
                 (dist^2 (+ (* dx dx) (* dy dy)))
                 (sum-r (+ ra (radius b))))
            (when (< *epsilon* dist^2 (* sum-r sum-r))
              (let ((dv.dp (+ (* (- (v:x vb) (v:x va)) dx)
                              (* (- (v:y vb) (v:y va)) dy))))
                ;; Only trade momentum when the pair is actually closing.  One
                ;; that overlaps but is already separating would otherwise be
                ;; flipped a second time, which is a pure energy source -- and
                ;; in a basin, where balls pile up, that fires nonstop.
                (when (minusp dv.dp)
                  (let ((s (/ dv.dp dist^2)))
                    (incf (v:x va) (* s dx))
                    (incf (v:y va) (* s dy))
                    (decf (v:x vb) (* s dx))
                    (decf (v:y vb) (* s dy)))))
              ;; Separate them symmetrically along the line of centres.  This
              ;; is a correction, not motion, so it goes through SHIFT-BALL
              ;; to keep it from doing free work against the field.
              (let* ((dist (sqrt dist^2))
                     (push (/ (- (+ sum-r *epsilon*) dist)
                              (* 2 dist))))
                (shift-ball a (- (* push dx)) (- (* push dy)))
                (shift-ball b (* push dx) (* push dy)))
              (put-ball-in-bucket buckets bucket-grid-coord-fn a)
              (put-ball-in-bucket buckets bucket-grid-coord-fn b))))))))

(defun reconcile-collisions-between-buckets (as bs buckets bucket-grid-coord-fn)
  (if (eql as bs)
      (loop :for (a . rest) :on as
            :do (reconcile-collisions a rest buckets bucket-grid-coord-fn))
      (loop :for a :in as
            :do (reconcile-collisions a bs buckets bucket-grid-coord-fn))))

(defun reconcile-collisions-in-buckets (buckets bucket-grid-coord-fn)
  (let ((bw (array-dimension buckets 0))
        (bh (array-dimension buckets 1))
        (cmps 0))
    (flet ((dorow (bx ox)
             (loop :for by :from 0 :below bh
                   :for as := (aref buckets bx by)
                   :do (let ((bs (aref buckets ox by)))
                         (reconcile-collisions-between-buckets as bs buckets bucket-grid-coord-fn)
                         (incf cmps (* (length as) (length bs))))
                   :when (plusp by)
                     :do (let ((bs (aref buckets ox (1- by))))
                           (reconcile-collisions-between-buckets as bs buckets bucket-grid-coord-fn)
                           (incf cmps (* (length as) (length bs))))
                   :when (< (1+ by) bh)
                     :do (let ((bs (aref buckets ox (1+ by))))
                           (reconcile-collisions-between-buckets as bs buckets bucket-grid-coord-fn)
                           (incf cmps (* (length as) (length bs)))))))
      (loop :for bx :from 0 :below bw
            :do (dorow bx bx)
            :when (plusp bx)
              :do (dorow bx (1- bx))
            :when (< (1+ bx) bw)
              :do (dorow bx (1+ bx)))
      #+(or)
      (format *debug-io* "~A~%" cmps))))

(defun make-buckets ()
  (let* ((width (ceiling *grid-scale* (+ (* *ball-radius* 2) *epsilon*)))
         (width (if (oddp width)
                    (1+ width)
                    width))
         (scale (/ *grid-scale* width))
         (padded-width (+ width 4))
         (offset (/ padded-width 2)))
    (values (make-array (list padded-width padded-width)
                        :initial-element nil)
            (lambda (x)
              (+ (round x scale)
                 offset)))))

(defun update-balls (dt)
  "Advance every ball by DT seconds."
  (multiple-value-bind (buckets bucket-grid-coord-fn) (make-buckets)
    (dolist (b *balls*)
      (update-ball b dt)
      (put-ball-in-bucket buckets bucket-grid-coord-fn b t))
    (reconcile-collisions-in-buckets buckets bucket-grid-coord-fn)
    ))

(defun momentum (b)
  (v3:length (vel b)))

(defun kinetic-energy (b)
  ;; XY only, to match the dynamics.
  (let ((v (vel b)))
    (/ (+ (* (v:x v) (v:x v))
          (* (v:y v) (v:y v)))
       2.0)))

(defun potential-energy (b)
  ;; a = g * grad h = -grad U, so U = -g * h.  H is the height of the field
  ;; under the ball, read from the field rather than from POS.Z so that this is
  ;; right even when called between a collision and the next snap.
  (let ((pos (pos b)))
    (* (- *gravitational-force*)
       (influence-at (v:x pos) (v:y pos)))))

(defun energy (b)
  (+ (kinetic-energy b)
     (potential-energy b)))

(defun total-momentum ()
  (reduce #'+ *balls* :key #'momentum :initial-value 0))

(defun total-energy ()
  (reduce #'+ *balls* :key #'energy :initial-value 0.0))
