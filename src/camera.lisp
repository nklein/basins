;;; camera.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Camera
;;;; ------------------------------------------------------------------

(defun world->cam ()
  (m4:look-at *camera-up* *camera-position* *camera-target*))

(defun cam->clip ()
  ;; Recomputed every frame from the live viewport, so a resized window needs
  ;; no callback.  If resizing ever looks letterboxed, CEPL's default viewport
  ;; is not tracking the window and you'll want a skitter window-size event.
  (let* ((resolution (viewport-resolution (current-viewport)))
         (width (float (aref resolution 0) 1.0))
         (height (float (aref resolution 1) 1.0)))
    (rtg-math.projection:perspective width height
                                     *camera-near* *camera-far* *camera-fov*)))

(defun normal-matrix (model->world)
  "The transform to apply to normals under MODEL->WORLD.

For the translation and uniform-scale matrices used here this reduces to the
identity, but writing out the inverse-transpose means that rotating or
non-uniformly scaling an object later will not silently break the shading."
  (m4:to-mat3 (m4:transpose (m4:affine-inverse model->world))))
