;;; state.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; State
;;;; ------------------------------------------------------------------

(defparameter *grid-influences* (list
                                      (make-instance 'fn-gaussian :x0 8
                                                                  :y0 -3
                                                                  :scale -3
                                                                  :sigma^2 (/ *grid-scale* 2))
                                      (make-instance 'fn-gaussian :x0 -7
                                                                  :y0 2
                                                                  :scale -4
                                                                  :sigma^2 (* *grid-scale* 2))

                                      (make-instance 'fn-sinc :scale 5 :x0 -12 :y0 0 :freq 1/2)
                                      #+(or)
                                      (make-instance 'fn-sinc :scale -3 :x0 8 :y0 -3 :freq 9/10)))


(defparameter *field-vertices* nil)
(defparameter *field-indices* nil)
(defparameter *field-stream* nil)

;; One unit sphere, shared by every ball.
(defparameter *ball-vertices* nil)
(defparameter *ball-indices* nil)
(defparameter *ball-stream* nil)

(defparameter *balls* nil)

(defparameter *running* nil)
(defparameter *last-frame-time* nil)

;; Mouse position as of the last frame of an in-progress drag, or NIL when no
;; button is held.  Resetting it to NIL on release is what stops the next
;; press from being read as one huge jump.
(defparameter *drag-anchor* nil)
