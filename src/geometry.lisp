;;; geometry.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Geometry
;;;; ------------------------------------------------------------------

(setf *grid-influences* (list (make-instance 'fn-gaussian :scale -10
                                                          :sigma^2 (* 3 *grid-scale*))
                              (make-instance 'fn-sinc :scale 5 :x0 2)))

(defun influence-at (x y)
  (reduce (lambda (acc fn)
            (+ acc
               (f fn x y)))
          *grid-influences* :initial-value 0.0))

(defun dinfluence/dx-at (x y)
  (reduce (lambda (acc fn)
            (+ acc
               (df/dx fn x y)))
          *grid-influences* :initial-value 0.0))

(defun dinfluence/dy-at (x y)
  (reduce (lambda (acc fn)
            (+ acc
               (df/dy fn x y)))
          *grid-influences* :initial-value 0.0))

(defun tangent-x (x y)
  (let ((s (dinfluence/dx-at x y)))
    (v3:normalize (v! 1 0 s))))

(defun tangent-y (x y)
  (let ((s (dinfluence/dy-at x y)))
    (v3:normalize (v! 0 1 s))))

(defun sinpart (cis)
  (imagpart cis))

(defun cospart (cis)
  (realpart cis))

(defun %grid-height (x y)
  (influence-at x y))

(defun %grid-point (a b)
  (flet ((index-to-coordinate (a max)
           (* *grid-scale* (/ (- a (/ max 2))
                              max))))
    (let ((x (index-to-coordinate a *grid-width*))
          (y (index-to-coordinate b *grid-depth*)))
      (let ((z (%grid-height x y)))
        (v! x y z)))))

#|
       |---- w ----|
    -  +--+--+--+--+
    |  | /| /| /| /|
    |  |/ |/ |/ |/ |
    d  +--+--+--+--+
    |  | /| /| /| /|
    |  |/ |/ |/ |/ |
    -  +--+--+--+--+
|#
(defun %grid-index (w d)
  (+ (* *grid-width* d)
     w))

(defun %sheet-normal (x y)
  (v3:cross (tangent-x x y)
            (tangent-y x y)))

(defun %random-normal ()
  (let ((theta (* 1/2 pi (- (random 1.0) 0.5)))
        (phi (* 2 pi (random 1.0))))
    (let ((cis-theta (cis theta))
          (cis-phi (cis phi)))
      (v! (* (cospart cis-phi) (sinpart cis-theta))
          (* (sinpart cis-phi) (sinpart cis-theta))
          (cospart cis-theta)))))

(defun heightfield-mesh ()
  "Return (values VERTICES INDICES) for the terrain.

VERTICES is a list of (POSITION NORMAL) pairs, one per grid vertex, each a
vec3 in world space.  INDICES is a flat list of vertex indices, three per
triangle, wound counter-clockwise seen from above."
  ;; TODO: build the grid.
  (let ((points (loop :for d :from 0 :below *grid-depth*
                      :appending (loop :for w :from 0 :below *grid-width*
                                       :collecting (%grid-point w d))))
        (indices (loop :for d :from 1 :below *grid-depth*
                       :appending (loop :for w :from 1 :below *grid-width*
                                        :appending (list (%grid-index (1- w)     d)
                                                         (%grid-index (1- w) (1- d))
                                                         (%grid-index     w  (1- d)))
                                        :appending (list (%grid-index (1- w)     d)
                                                         (%grid-index     w  (1- d))
                                                         (%grid-index     w      d))))))
    (values (mapcar (lambda (p)
                      (list p
                            (%sheet-normal (v:x p) (v:y p))))
                    points)
            indices)))

(defun %lat-angle (lat)
  (* pi (/ lat *ball-lat-lines*)))

(defun %long-angle (long)
  (* 2 pi (/ long *ball-long-lines*)))

(defun %ball-point (lat long)
  (let ((theta (%lat-angle lat))
        (phi (%long-angle (if (evenp lat)
                              long
                              (+ long 1/2)))))
    (let ((cis-theta (cis theta))
          (cis-phi (cis phi)))
      (v! (* (cospart cis-phi) (sinpart cis-theta))
          (* (sinpart cis-phi) (sinpart cis-theta))
          (cospart cis-theta)))))

(defun ball-mesh ()
  "Return (values VERTICES INDICES) for a unit sphere centered on the origin.

Every ball is drawn from this one mesh, scaled by its radius and translated
into place, so each vertex's normal is simply its position."
  (let ((points (append (list (v! 0 0  1))
                        (loop :for lat :from 1 :below (1- *ball-lat-lines*)
                              :appending (loop :for long :from 0 :below *ball-long-lines*
                                               :collecting (%ball-point lat long)))
                        (list (v! 0 0 -1))))
        (indices (list 0 1 2)))
    (values (mapcar #'list points points)
            indices)))

(defun upload-mesh (vertices indices)
  "Push VERTICES and INDICES to the GPU.
Returns (values GPU-VERTICES GPU-INDICES STREAM)."
  (let* ((gpu-vertices (make-gpu-array vertices :element-type 'g-pn))
         (gpu-indices (make-gpu-array indices :element-type :unsigned-int))
         (stream (make-buffer-stream gpu-vertices :index-array gpu-indices)))
    (values gpu-vertices gpu-indices stream)))
