;;; functions.lisp

(in-package :basins)

(declaim (inline %r^2))
(defun %r^2 (dx dy)
  (+ (* dx dx)
     (* dy dy)))

(declaim (inline %r))
(defun %r (dx dy)
  (sqrt (%r^2 dx dy)))

(defclass fn ()
  ())

(defgeneric f (fn x y))
(defgeneric df/dx (fn x y))
(defgeneric df/dy (fn x y))

(defclass fn-gaussian (fn)
  ((x0 :reader x0 :initarg :x0)
   (y0 :reader y0 :initarg :y0)
   (scale :reader scale :initarg :scale)
   (sigma^2 :reader sigma^2 :initarg :sigma^2))
  (:default-initargs :x0 0.0 :y0 0.0 :scale 1.0 :sigma^2 1.0))

(defmethod f ((fn fn-gaussian) x y)
  (with-accessors ((x0 x0)
                   (y0 y0)
                   (scale scale)
                   (sigma^2 sigma^2)) fn
    (let ((r^2 (%r^2 (- x x0) (- y y0))))
      (* scale
         (exp (- (/ r^2 sigma^2)))))))

(defmethod df/dx ((fn fn-gaussian) x y)
  ;; r^2 = (x-x0)^2 + (y-y0)^2
  ;; dr^2/dx = 2(x-x0)
  ;; u = -r^2 / sigma^2
  ;; f = scale * e^u
  ;; df/dx = f * du/dx
  ;; du/dx = - dr^2/dx / sigma^2 = -2(x-x0)/sigma^2
  (with-accessors ((x0 x0)
                   (sigma^2 sigma^2)) fn
    (let ((du/dx (/ (* -2 (- x x0)) sigma^2)))
      (* (f fn x y) du/dx))))

(defmethod df/dy ((fn fn-gaussian) x y)
  ;; r^2 = (x-x0)^2 + (y-y0)^2
  ;; dr^2/dy = 2(y-y0)
  ;; u = -r^2 / sigma^2
  ;; f = scale * e^u
  ;; df/dy = f * du/dy
  ;; du/dy = - dr^2/dy / sigma^2 = -2(y-y0)/sigma^2
  (with-accessors ((y0 y0)
                   (sigma^2 sigma^2)) fn
    (let ((du/dy (/ (* -2 (- y y0)) sigma^2)))
      (* (f fn x y) du/dy))))

(defclass fn-sinc (fn)
  ((x0 :reader x0 :initarg :x0)
   (y0 :reader y0 :initarg :y0)
   (scale :reader scale :initarg :scale)
   (freq :reader freq :initarg :freq))
  (:default-initargs :x0 0.0 :y0 0.0 :scale 1.0 :freq 1.0))

(defmethod f ((fn fn-sinc) x y)
  (with-accessors ((x0 x0)
                   (y0 y0)
                   (scale scale)
                   (freq freq)) fn
    (let ((r (%r (- x x0) (- y y0))))
      (if (< *epsilon* r)
          (/ (* scale
                (sin (* freq r)))
             (* freq r))
          scale))))

(defmethod df/dx ((fn fn-sinc) x y)
  ;; r^2 = (x-x0)^2 + (y-y0)^2
  ;; v = sqrt(r^2)
  ;; f = scale * sin( freq * v ) / v
  ;; df/dx = ( scale * cos( freq * v ) * freq * dv/dx * v - f * dv/dx ) / v^2
  ;; dv/dx = 1/2 (1/sqrt(r^2)) dr^2/dx
  ;; dr^2/dx = 2(x-x0)
  (with-accessors ((x0 x0)
                   (y0 y0)
                   (scale scale)
                   (freq freq)) fn
    (let* ((r (%r (- x x0) (- y y0)))
           (cis-theta (cis (* freq r)))
           )
      (cond
        ((< *epsilon* r)
         (/ (* scale (- (* (cospart cis-theta)
                           (- x x0))
                        (/ (* (sinpart cis-theta)
                              (- x x0))
                           r)))
            (* freq freq r r)))
        (t
         0.0)))))

(defmethod df/dy ((fn fn-sinc) x y)
  (with-accessors ((x0 x0)
                   (y0 y0)
                   (scale scale)
                   (freq freq)) fn
    (let* ((r (%r (- x x0) (- y y0)))
           (cis-theta (cis (* freq r))))
      (cond
        ((< *epsilon* r)
         (/ (* scale (- (* (cospart cis-theta)
                           (- y y0))
                        (/ (* (sinpart cis-theta)
                              (- y y0))
                           r)))
            (* freq freq r r)))
        (t
         0.0)))))
