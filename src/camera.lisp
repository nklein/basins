;;; camera.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Camera
;;;; ------------------------------------------------------------------

(defun world->cam ()
  (m4:look-at *camera-up* *camera-position* *camera-target*))

(defun cam->clip ()
  "The projection matrix for the viewport as it stands right now.

*CAMERA-FOV* is the *horizontal* field of view: that angular slice of the
world spans the window's full width whatever shape the window is.  A window
made taller therefore reveals more above and below rather than narrowing what
can be seen across it.

RTG-MATH's PERSPECTIVE wants a vertical fov, so we convert.  Both halves of
the frustum share the same focal distance d, giving

    tan(vfov/2) = (h/2)/d = tan(hfov/2) * h/w"
  (let* ((resolution (viewport-resolution (current-viewport)))
         (width (aref resolution 0))
         (height (aref resolution 1)))
    ;; A minimised window reports a zero dimension; don't divide by it.
    (if (and (plusp width) (plusp height))
        (let* ((half-horizontal (tan (/ (radians-f *camera-fov*) 2.0)))
               (vertical-fov (* 2.0 (atan (* half-horizontal
                                            (/ height width))))))
          (rtg-math.projection:perspective-radian-fov
           width height *camera-near* *camera-far* vertical-fov))
        (m4:identity))))

;;;; ------------------------------------------------------------------
;;;; Orbiting
;;;; ------------------------------------------------------------------

(defun orbit-camera (delta-azimuth delta-elevation)
  "Swing the camera around *CAMERA-TARGET* by the given angles, in degrees.

The distance to the target is preserved.  DELTA-AZIMUTH turns the camera
about the world z-axis; DELTA-ELEVATION raises or lowers it within the
vertical plane holding the z-axis and the camera, bounded below by
*CAMERA-LOWER-PITCH-LIMIT* and above by *CAMERA-UPPER-PITCH-LIMIT*."
  (let* ((offset (v3:- *camera-position* *camera-target*))
         (radius (v3:length offset)))
    (when (plusp radius)
      (let* ((azimuth (+ (atan (aref offset 1) (aref offset 0))
                         (radians-f delta-azimuth)))
             (lower-limit (radians-f *camera-lower-pitch-limit*))
             (upper-limit (radians-f *camera-upper-pitch-limit*))
             ;; Rounding can push the ratio a hair outside [-1,1], where ASIN
             ;; would hand back a complex number.
             (elevation (clamp lower-limit upper-limit
                               (+ (asin (clamp -1.0 1.0
                                               (/ (aref offset 2) radius)))
                                  (radians-f delta-elevation))))
             (ground-radius (* radius (cos elevation))))
        (setf *camera-position*
              (v3:+ *camera-target*
                    (v! (* ground-radius (cos azimuth))
                        (* ground-radius (sin azimuth))
                        (* radius (sin elevation)))))))))

(defun update-camera-from-mouse ()
  "Orbit the camera for as long as the left mouse button is held."
  (let ((position (mouse-pos (mouse 0))))
    (if (mouse-down-p mouse.left)
        (progn
          (when *drag-anchor*
            (let ((dx (- (aref position 0) (aref *drag-anchor* 0)))
                  (dy (- (aref position 1) (aref *drag-anchor* 1))))
              ;; DX is negated so the terrain follows the cursor sideways
              ;; rather than fleeing it.  DY is not: screen y grows downward,
              ;; so leaving it alone means dragging down lifts the camera.
              ;; Flip either sign to invert that axis.
              (orbit-camera (* (- dx) *camera-orbit-speed*)
                            (* dy *camera-orbit-speed*))))
          ;; Copy: skitter hands out the vector it stores.
          (setf *drag-anchor* (v! (aref position 0) (aref position 1))))
        (setf *drag-anchor* nil))))

;;;; ------------------------------------------------------------------
;;;; Viewport
;;;; ------------------------------------------------------------------

(defun resize-viewport (size &rest ignored)
  "Match the viewport to SIZE, a uvec2 of pixels.

Setting the default viewport's resolution is enough on its own: CEPL issues
the glViewport call and resizes the default framebuffer's attachments to
match.  CAM->CLIP reads the viewport afresh each frame, so the projection
follows along without being told."
  (declare (ignore ignored))
  (setf (viewport-resolution (current-viewport))
        (v! (float (aref size 0) 1.0)
            (float (aref size 1) 1.0))))

(defun sync-viewport ()
  "Match the viewport to the window as it is at this moment.

Skitter reports size *changes*, so a window resized while we were not
listening would otherwise go unnoticed until the next resize."
  (let ((size (window-size (window 0))))
    (when size
      (resize-viewport size))))

(defun normal-matrix (model->world)
  "The transform to apply to normals under MODEL->WORLD.

For the translation and uniform-scale matrices used here this reduces to the
identity, but writing out the inverse-transpose means that rotating or
non-uniformly scaling an object later will not silently break the shading."
  (m4:to-mat3 (m4:transpose (m4:affine-inverse model->world))))
