;;; setup-cleanup.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Setup and teardown
;;;; ------------------------------------------------------------------

(defun free-resources ()
  (dolist (object (list *field-stream* *field-vertices* *field-indices*
                        *ball-stream* *ball-vertices* *ball-indices*
                        *shadow-sampler* *shadow-fbo* *shadow-texture*))
    (when object (free object)))
  (setf *field-stream* nil *field-vertices* nil *field-indices* nil
        *ball-stream* nil *ball-vertices* nil *ball-indices* nil
        *shadow-sampler* nil *shadow-fbo* nil *shadow-texture* nil))

(defun init-shadow-map ()
  "Build the depth-only framebuffer the light renders into.

The sampler filters with :NEAREST deliberately.  The texture has no mipmaps,
so CEPL's default :LINEAR-MIPMAP-LINEAR would leave it incomplete and
sampling would come back black; the softening is done by the nine taps in
SHADOW-VISIBILITY instead."
  (setf *shadow-texture*
        (make-texture nil
                      :dimensions (list *shadow-map-size* *shadow-map-size*)
                      :element-type :depth-component24)
        *shadow-fbo*
        (make-fbo (list :d *shadow-texture*))
        *shadow-sampler*
        (sample *shadow-texture*
                :minify-filter :nearest
                :magnify-filter :nearest
                :wrap :clamp-to-edge)))

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
  (init-shadow-map)
  (setf *balls* (make-initial-balls)
        *last-frame-time* nil))
