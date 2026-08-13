;;; balls.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Balls
;;;; ------------------------------------------------------------------

(defstruct ball
  (position (v! 0.0 0.0 0.0))
  (velocity (v! 0.0 0.0 0.0))
  (radius 1.0)
  (color (v! 0.8 0.3 0.2)))

(defun make-initial-balls ()
  "Return the list of BALLs the simulation starts with."
  ;; TODO: decide how many, where, and how big.
  nil)

(defun update-balls (dt)
  "Advance every ball by DT seconds."
  (declare (ignorable dt))
  ;; TODO: integrate each ball against the heightfield.
  nil)
