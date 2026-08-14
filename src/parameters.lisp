;;;; parameters.lisp

(in-package #:basins)

;;;; Conventions
;;;;
;;;; World space is Z-up: the heightfield lies in the XY plane and the height
;;;; of a sample is its Z coordinate.  Triangles are wound counter-clockwise
;;;; when seen from above.

;;;; ------------------------------------------------------------------
;;;; Tunables
;;;; ------------------------------------------------------------------

(defparameter *clear-color* (v! 0.08 0.09 0.12 1.0))

;; A single directional "sun".  This is the direction pointing *towards* the
;; light, not the direction the light travels.  It is normalised in the shader.
(defparameter *light-direction* (v! -0.1 2.3 7.2))
(defparameter *light-color* (v! 0.3 0.3 0.25))
(defparameter *ambient-color* (v! 0.22 0.23 0.26))

(defparameter *field-albedo* (v! 0.40 0.65 0.40))
(defparameter *field-shininess* 10.0)
(defparameter *ball-shininess* 32.0)

(defparameter *camera-position* (v! 15 25.0 25.0))
(defparameter *camera-target* (v! 0.0 0.0 0.0))
(defparameter *camera-up* (v! 0.0 0.0 1.0))
(defparameter *camera-fov* 60.0)
(defparameter *camera-near* 0.1)
(defparameter *camera-far* 500.0)

;; Degrees the camera swings per pixel of mouse drag.
(defparameter *camera-orbit-speed* 0.4)

;; How low the camera may be swung.  Well above zero, so that the view stays
;; on the heightfield from above rather than grazing it edge-on or dropping
;; underneath it.
(defparameter *camera-lower-pitch-limit* 1.0)

;; How near the camera may come to straight overhead.  At exactly 90 degrees
;; the view direction is parallel to *CAMERA-UP* and LOOK-AT has no way to
;; decide which way up the picture goes.
(defparameter *camera-upper-pitch-limit* 89.0)

;; Longest simulation step we are willing to take.  Without this, pausing at
;; the REPL and resuming hands the integrator one enormous dt.
(defparameter *max-timestep* 0.1)

;; Simulation substep.  The integrator is symplectic, which bounds its energy
;; error only at a *fixed* step; handed a step that varies with the framerate
;; the error random-walks instead.  So real time is accumulated and paid out in
;; whole substeps of this size.
(defparameter *fixed-timestep* (/ 1.0 240.0))

(defparameter *epsilon* 0.00001)

(defparameter *grid-width* 255)
(defparameter *grid-depth* 255)
(defparameter *grid-scale* 40.0)
(defparameter *gravitational-force* -2.0)

(defparameter *ball-lat-lines* 10)
(defparameter *ball-long-lines* 20)
(defparameter *ball-radius* 0.5)

;;;; ------------------------------------------------------------------
;;;; Shadows
;;;;
;;;; These come after *GRID-SCALE* because the light's box is sized from it.
;;;; ------------------------------------------------------------------

;; Edge length of the square depth map rendered from the light's viewpoint.
;; Larger is sharper and costs memory; this is the first knob to turn if the
;; shadow edges look blocky.
(defparameter *shadow-map-size* 2048)

;; Half-width of the light's orthographic box.  It has to enclose everything
;; that can cast, and the field's corners sit at *GRID-SCALE* / 2 times the
;; square root of two, so this leaves a little margin over that.
(defparameter *shadow-extent* (* *grid-scale* 0.75))

;; How far back along the light direction the light's eye sits, and the depth
;; range in front of it.  Only needs to be far enough that the whole field is
;; between the near and far planes.
(defparameter *shadow-eye-distance* 60.0)
(defparameter *shadow-near* 1.0)
(defparameter *shadow-far* 120.0)

;; Slack subtracted from a fragment's depth before comparing it against the
;; map, to stop surfaces shadowing themselves in stripes ("shadow acne").
;; Surfaces lit at a grazing angle span more depth per texel and so need
;; more; the pair is interpolated by the angle.  Too much detaches a shadow
;; from whatever cast it, so raise these only until the stripes go.
(defparameter *shadow-bias-min* 0.0015)
(defparameter *shadow-bias-max* 0.008)

;; Spacing between neighbouring shadow taps, counted in map texels.  The
;; filter is a fixed 5x5 grid, so this, not the tap count, is what sets how
;; wide the soft edge is.
;;
;; At 1.0 the taps are adjacent and the whole kernel spans four texels, which
;; over this light's box works out at under a tenth of a ball radius and is
;; invisible.  Raise it to widen the penumbra.  Push it much past 4 or 5 and
;; the fixed grid starts to show as banding rather than a smooth gradient.
(defparameter *shadow-softness* 3.5)
