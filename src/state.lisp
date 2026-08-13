;;; state.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; State
;;;; ------------------------------------------------------------------

(defparameter *grid-influences* nil)

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
