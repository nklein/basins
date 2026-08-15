;;; geometry.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Geometry
;;;; ------------------------------------------------------------------

(defun influence-at (x y)
  (reduce (lambda (acc fn)
            (+ acc
               (f fn x y)))
          *grid-influences* :initial-value 0.0))

(defun dinfluence-at (x y)
  (reduce (lambda (acc fn)
            (v2:+ acc
                  (gradient fn x y)))
          *grid-influences*
          :initial-value (v! 0.0 0.0)))

(defun tangents (x y)
  (let ((s (dinfluence-at x y)))
    (values (v3:normalize (v! 1 0 (v:x s)))
            (v3:normalize (v! 0 1 (v:y s))))))

(declaim (inline sinpart))
(defun sinpart (cis)
  (imagpart cis))

(declaim (inline cospart))
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
  (multiple-value-bind (tx ty) (tangents x y)
    (v3:cross tx ty)))

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
                        (loop :for lat :from 1 :below *ball-lat-lines*
                              :appending (loop :for long :from 0 :below *ball-long-lines*
                                               :collecting (%ball-point lat long)))
                        (list (v! 0 0 -1))))
        (indices (append
                  ;; north cap
                  (loop :with offset := 1
                        :with pole := 0
                        :for long :from 0 :below *ball-long-lines*
                        :for next := (mod (1+ long) *ball-long-lines*)
                        :collecting pole
                        :collecting (+ long offset)
                        :collecting (+ next offset))
                  #|
                      *---*---*---*    even
                       \ / \ / \ / \
                        *---*---*---*  odd
                       / \ / \ / \ /
                      *---*---*---*    even
                  |#
                  ;; strips
                  (loop :for lat :from 1 :below (1- *ball-lat-lines*)
                        :appending (loop :with offset := (1+ (* (1- lat)
                                                                *ball-long-lines*))
                                         :for long :from 0 :below *ball-long-lines*
                                         :for next := (mod (1+ long) *ball-long-lines*)
                                         :when (oddp lat)
                                           :appending (list (+ long *ball-long-lines* offset)
                                                            (+ next *ball-long-lines* offset)
                                                            (+ long offset)
                                                            (+ next *ball-long-lines* offset)
                                                            (+ next offset)
                                                            (+ long offset))
                                         :when (evenp lat)
                                           :appending (list (+ long offset)
                                                            (+ long *ball-long-lines* offset)
                                                            (+ next offset)
                                                            (+ long *ball-long-lines* offset)
                                                            (+ next *ball-long-lines* offset)
                                                            (+ next offset))))
                  ;; south cap
                  (loop :with offset := (+ 1 (* (- *ball-lat-lines* 2)
                                                *ball-long-lines*))
                        :with pole := (+ *ball-long-lines* offset)
                        :for long :from 0 :below *ball-long-lines*
                        :for next := (mod (1+ long) *ball-long-lines*)
                        :collecting (+ next offset)
                        :collecting (+ long offset)
                        :collecting (+ pole)))))
    (values (mapcar #'list points points)
            indices)))

(defun upload-mesh (vertices indices)
  "Push VERTICES and INDICES to the GPU.
Returns (values GPU-VERTICES GPU-INDICES STREAM)."
  (let* ((gpu-vertices (make-gpu-array vertices :element-type 'g-pn))
         (gpu-indices (make-gpu-array indices :element-type :unsigned-int))
         (stream (make-buffer-stream gpu-vertices :index-array gpu-indices)))
    (values gpu-vertices gpu-indices stream)))
