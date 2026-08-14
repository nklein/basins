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
(defparameter *light-direction* (v! -0.3 0.6 2.2))
(defparameter *light-color* (v! 0.3 0.3 0.25))
(defparameter *ambient-color* (v! 0.12 0.13 0.16))

(defparameter *field-albedo* (v! 0.40 0.65 0.40))
(defparameter *field-shininess* 10.0)
(defparameter *ball-shininess* 64.0)

(defparameter *camera-position* (v! 15 25.0 10.0))
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

(defparameter *epsilon* 0.00001)

(defparameter *grid-width* 255)
(defparameter *grid-depth* 255)
(defparameter *grid-scale* 30.0)

(defparameter *ball-lat-lines* 10)
(defparameter *ball-long-lines* 20)
(defparameter *ball-radius* 0.5)
