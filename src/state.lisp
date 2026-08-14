;;; state.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; State
;;;; ------------------------------------------------------------------

(defparameter *grid-influences* (list (make-instance 'fn-gaussian :x0 -8
                                                                  :y0 2
                                                                  :scale -6
                                                                  :sigma^2 (* *grid-scale* 1))

                                      (make-instance 'fn-gaussian :x0 -18
                                                                  :y0 18
                                                                  :scale -3
                                                                  :sigma^2 *grid-scale*)

                                      (make-instance 'fn-sinc :x0 8
                                                              :y0 5
                                                              :scale -4
                                                              :freq 1/3)

                                      (make-instance 'fn-sinc :scale 8 :x0 -12 :y0 0 :freq 1/2)))


(defparameter *field-vertices* nil)
(defparameter *field-indices* nil)
(defparameter *field-stream* nil)

;; One unit sphere, shared by every ball.
(defparameter *ball-vertices* nil)
(defparameter *ball-indices* nil)
(defparameter *ball-stream* nil)

;; Shadow mapping: a depth-only framebuffer rendered from the light, the
;; texture behind it, and the sampler the beauty pass reads it through.
(defparameter *shadow-texture* nil)
(defparameter *shadow-fbo* nil)
(defparameter *shadow-sampler* nil)

(defparameter *balls* nil)

(defparameter *running* nil)
(defparameter *last-frame-time* nil)

;; Mouse position as of the last frame of an in-progress drag, or NIL when no
;; button is held.  Resetting it to NIL on release is what stops the next
;; press from being read as one huge jump.
(defparameter *drag-anchor* nil)
