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
   (acceleration :accessor acc :initarg :acceleration)
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

(defun update-ball (b dt)
  (let ((gforce *gravitational-force*)
        (scale/2 (/ *grid-scale* 2)))
    (with-accessors ((pos pos) (vel vel)) b
      (let ((x (v:x pos))
            (y (v:y pos)))
        (let ((d/dx (dinfluence/dx-at x y))
              (d/dy (dinfluence/dy-at x y)))
          (setf vel (v3:+ vel
                          (v! (* gforce dt d/dx)
                              (* gforce dt d/dy)
                              0))))
        (setf pos (v3:+ pos
                        (v3:*s vel dt)))
        ;; bounce at edges
        (unless (< (- scale/2) x scale/2)
          (setf (v:x pos) (clamp (- *epsilon* scale/2) (- scale/2 *epsilon*) x)
                (v:x vel) (- (v:x vel)))
          #+ (or) (format *debug-io* "~A~%" vel))
        (unless (< (- scale/2) y scale/2)
          (setf (v:y pos) (clamp (- *epsilon* scale/2) (- scale/2 *epsilon*) y)
                (v:y vel) (- (v:y vel))))
        ;; stay on surface
        (setf (v:z pos) (+ (influence-at (v:x pos) (v:y pos))
                           (radius b)))))))

(defun reconcile-collisions ()
  ;; since all balls are the same mass and perfectly elastic, they just swap velocities
  ;; ...or they would if they were point particles, but as they are bigger than all that,
  ;;    we are going to have to do more...
  (dolist (a *balls*)
    (with-accessors ((pa pos) (va vel) (ra radius)) a
      (dolist (b *balls*)
        (unless (eq a b)
          (with-accessors ((pb pos) (vb vel) (rb radius)) b
            (let* ((pb-pa (v3:- pb pa))
                   (dist (v3:length pb-pa)))
              (unless (<= (+ ra rb) dist)
                (let* ((dir (v3:normalize pb-pa))
                       (mid (v3:*s (v3:+ pa pb) 0.5))
                       (vb-va (v3:- vb va))
                       (dist^2 (* dist dist))
                       (dv.dp (v3:dot vb-va pb-pa))
                       (scaled-pb-pa (v3:*s pb-pa (/ dv.dp dist^2))))
                  (setf va (v3:+ va scaled-pb-pa)
                        vb (v3:- vb scaled-pb-pa))
                  (setf pa (v3:- mid
                                 (v3:*s dir (+ ra (/ *epsilon* 2))))
                        pb (v3:+ mid
                                 (v3:*s dir (+ rb (/ *epsilon* 2))))))))))))))

(defun update-balls (dt)
  "Advance every ball by DT seconds."
  (dolist (b *balls*)
    (update-ball b dt))
  (reconcile-collisions))


(defun momentum (b)
  (let ((v (vel b)))
    (v3:length v)))

(defun kinetic-energy (b)
  (let ((v (vel b)))
    (/ (v3:dot v v) 2.0)))

(defun potential-energy (b)
  (let ((h (v:z (vel b))))
    (* *gravitational-force* h)))

(defun energy (b)
  (+ (kinetic-energy b)
     (potential-energy b)))

(defun total-momentum ()
  (reduce #'+ *balls* :key #'momentum :initial-value 0.0))

(defun total-energy ()
  (reduce #'+ *balls* :key #'energy :initial-value 0.0))
