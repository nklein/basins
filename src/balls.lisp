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
   (color :reader color :initarg :color))
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
  (let ((cis-theta (* (random 0.125)
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

(defun update-ball (b dt)
  (let ((gforce *gravitational-force*)
        (limit (- (/ *grid-scale* 2) *epsilon*)))
    (with-accessors ((pos pos) (vel vel)) b
      ;; Symplectic Euler in the XY plane.  The surface enters only through its
      ;; gradient: a = g * grad h, which is -grad U for U = -g * h, so this is
      ;; a particle in a potential well and conserves 1/2|v|^2 + U exactly.
      (let ((d/dx (dinfluence/dx-at (v:x pos) (v:y pos)))
            (d/dy (dinfluence/dy-at (v:x pos) (v:y pos))))
        (setf vel (v3:+ vel
                        (v! (* gforce dt d/dx)
                            (* gforce dt d/dy)
                            0))))
      (setf pos (v3:+ pos
                      (v3:*s vel dt)))
      ;; Bounce at the edges.  Mirror the overshoot rather than clamping it
      ;; away: clamping teleports the ball to the wall, and on a slope that
      ;; jump in height is free potential energy.
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
      (snap-to-surface b))))

(defun reconcile-collisions ()
  ;; Since all balls are the same mass and perfectly elastic, a pair just
  ;; exchanges the component of relative velocity along the line of centres.
  ;;
  ;; All of it happens in XY.  The dynamics live in that plane, and letting Z
  ;; into the impulse leaves a phantom VEL.Z that never decays and never does
  ;; anything -- Z position is re-derived from the surface either way -- but
  ;; which every energy tally then counts.
  (dolist (a *balls*)
    (with-accessors ((pa pos) (va vel) (ra radius)) a
      (dolist (b *balls*)
        (unless (eq a b)
          (with-accessors ((pb pos) (vb vel) (rb radius)) b
            (let* ((dx (- (v:x pb) (v:x pa)))
                   (dy (- (v:y pb) (v:y pa)))
                   (dist^2 (+ (* dx dx) (* dy dy)))
                   (sum-r (+ ra rb)))
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
                ;; Separate them symmetrically along the line of centres, then
                ;; put both back on the field.
                (let* ((dist (sqrt dist^2))
                       (push (/ (- (+ sum-r *epsilon*) dist)
                                (* 2 dist))))
                  (decf (v:x pa) (* push dx))
                  (decf (v:y pa) (* push dy))
                  (incf (v:x pb) (* push dx))
                  (incf (v:y pb) (* push dy))
                  (snap-to-surface a)
                  (snap-to-surface b))))))))))

(defun update-balls (dt)
  "Advance every ball by DT seconds."
  (dolist (b *balls*)
    (update-ball b dt))
  (reconcile-collisions))


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
