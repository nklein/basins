;;; setup-cleanup.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Setup and teardown
;;;; ------------------------------------------------------------------

(defun free-resources ()
  (dolist (object (list *field-stream* *field-vertices* *field-indices*
                        *ball-stream* *ball-vertices* *ball-indices*))
    (when object (free object)))
  (setf *field-stream* nil *field-vertices* nil *field-indices* nil
        *ball-stream* nil *ball-vertices* nil *ball-indices* nil
        *grid-influences* nil))

(defun init ()
  (free-resources)                      ; so re-running does not leak
  (setf (clear-color) *clear-color*)
  (setf (depth-test-function) #'<)
  (multiple-value-bind (vertices indices) (heightfield-mesh)
    (setf (values *field-vertices* *field-indices* *field-stream*)
          (upload-mesh vertices indices)))
  (multiple-value-bind (vertices indices) (ball-mesh)
    (setf (values *ball-vertices* *ball-indices* *ball-stream*)
          (upload-mesh vertices indices)))
  (setf *balls* (make-initial-balls)
        *last-frame-time* nil))
