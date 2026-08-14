;;; state.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; State
;;;; ------------------------------------------------------------------

(defparameter *grid-influences* (list (make-instance 'fn-gaussian :scale -10
                                                                  :sigma^2 (* 3 *grid-scale*))
                                      (make-instance 'fn-sinc :scale 5 :x0 2)))


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
