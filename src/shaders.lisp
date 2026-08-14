;;; shaders.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Shaders
;;;; ------------------------------------------------------------------

;; Both the terrain and the balls use this pair; they differ only in their
;; uniforms.  G-PN is CEPL's built-in position+normal vertex struct, with
;; accessors POS and NORM.

(defun-g surface-vert ((vert g-pn)
                       &uniform
                       (model->world :mat4)
                       (world->cam :mat4)
                       (cam->clip :mat4)
                       (normal-matrix :mat3))
  (let ((world-position (* model->world (v! (pos vert) 1.0))))
    (values (* cam->clip (* world->cam world-position))
            ;; :smooth is the default, but say it out loud: interpolating the
            ;; per-vertex normal across the triangle is what makes the shading
            ;; smooth rather than faceted.
            (:smooth (s~ world-position :xyz))
            (:smooth (* normal-matrix (norm vert))))))

;;;; ------------------------------------------------------------------
;;;; Shadow lookup
;;;; ------------------------------------------------------------------

(defun-g shadow-test ((shadow-map :sampler-2d)
                      (uv :vec2)
                      (offset :vec2)
                      (reference :float))
  "1.0 where REFERENCE is nearer the light than what the map recorded."
  (if (<= reference (s~ (texture shadow-map (+ uv offset)) :x))
      1.0
      0.0))

(defun-g shadow-pcf ((shadow-map :sampler-2d)
                     (uv :vec2)
                     (reference :float)
                     (step :float))
  "Average of a 5x5 grid of shadow tests, STEP apart in texture space.

A single test answers yes or no, which gives a hard edge.  Averaging many of
them over a neighbourhood turns that into a gradient, and STEP is what sets
how wide the gradient is: the tap count fixes only how many distinct levels
the gradient has, not how far it reaches."
  (let ((sum 0.0))
    (dotimes (i 5)
      (dotimes (j 5)
        (let ((offset (* step (v! (float (- i 2))
                                  (float (- j 2))))))
          (setf sum (+ sum (shadow-test shadow-map uv offset reference))))))
    (/ sum 25.0)))

(defun-g shadow-visibility ((world-position :vec3)
                            (n-dot-l :float)
                            (world->light-clip :mat4)
                            (shadow-map :sampler-2d)
                            (bias-min :float)
                            (bias-max :float)
                            (step :float))
  "How much of the light reaches WORLD-POSITION: 0.0 shadowed, 1.0 lit."
  ;; The light's projection is orthographic, so w is always 1 and the usual
  ;; perspective divide can be skipped.
  (let* ((light-ndc (s~ (* world->light-clip (v! world-position 1.0)) :xyz))
         (coord (+ (* light-ndc 0.5) 0.5))
         (uv (s~ coord :xy))
         (depth (s~ coord :z))
         ;; Grazing surfaces span more depth per texel, so they need more
         ;; slack before they count as shadowing themselves.
         (bias (mix bias-max bias-min (clamp n-dot-l 0.0 1.0)))
         (reference (- depth bias)))
    (if (or (> depth 1.0)
            (< (s~ uv :x) 0.0) (> (s~ uv :x) 1.0)
            (< (s~ uv :y) 0.0) (> (s~ uv :y) 1.0))
        ;; Outside the light's box we have no information; call it lit, so
        ;; that anything beyond the field is not left mysteriously dark.
        1.0
        (shadow-pcf shadow-map uv reference step))))

(defun-g surface-frag ((world-position :vec3)
                       (world-normal :vec3)
                       &uniform
                       (eye-position :vec3)
                       (light-direction :vec3)
                       (light-color :vec3)
                       (ambient-color :vec3)
                       (albedo :vec3)
                       (shininess :float)
                       (world->light-clip :mat4)
                       (shadow-map :sampler-2d)
                       (shadow-bias-min :float)
                       (shadow-bias-max :float)
                       (shadow-step :float))
  ;; Blinn-Phong, evaluated in world space.
  (let* ((n (normalize world-normal))   ; interpolation shortens it; renormalise
         (l (normalize light-direction))
         (v (normalize (- eye-position world-position)))
         (h (normalize (+ l v)))
         (n-dot-l (dot n l))
         (diffuse (clamp n-dot-l 0.0 1.0))
         (specular (if (> diffuse 0.0)
                       (expt (clamp (dot n h) 0.0 1.0) shininess)
                       0.0))
         (visibility (shadow-visibility world-position n-dot-l
                                        world->light-clip shadow-map
                                        shadow-bias-min shadow-bias-max
                                        shadow-step))
         ;; Shadowing dims only what comes from the sun.  Ambient stands in
         ;; for light arriving from everywhere else, so a shadowed surface
         ;; should go dim rather than black.
         (color (+ (* ambient-color albedo)
                   (* visibility
                      (+ (* (* light-color albedo) diffuse)
                         (* light-color specular))))))
    (v! color 1.0)))

(defpipeline-g render-surface ()
  (surface-vert g-pn)
  (surface-frag :vec3 :vec3))

;;;; ------------------------------------------------------------------
;;;; Depth pass
;;;;
;;;; Renders the same geometry from the light's viewpoint.  Nothing is
;;;; shaded: the depth buffer is the entire product, so the fragment stage
;;;; has nothing to say and its output goes nowhere.
;;;; ------------------------------------------------------------------

(defun-g depth-vert ((vert g-pn)
                     &uniform
                     (model->world :mat4)
                     (world->light-clip :mat4))
  (* world->light-clip (* model->world (v! (pos vert) 1.0))))

(defun-g depth-frag ()
  (v! 0.0 0.0 0.0 1.0))

(defpipeline-g render-depth ()
  (depth-vert g-pn)
  (depth-frag))
