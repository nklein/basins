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

(defun-g surface-frag ((world-position :vec3)
                       (world-normal :vec3)
                       &uniform
                       (eye-position :vec3)
                       (light-direction :vec3)
                       (light-color :vec3)
                       (ambient-color :vec3)
                       (albedo :vec3)
                       (shininess :float))
  ;; Blinn-Phong, evaluated in world space.
  (let* ((n (normalize world-normal))   ; interpolation shortens it; renormalise
         (l (normalize light-direction))
         (v (normalize (- eye-position world-position)))
         (h (normalize (+ l v)))
         (diffuse (clamp (dot n l) 0.0 1.0))
         (specular (if (> diffuse 0.0)
                       (expt (clamp (dot n h) 0.0 1.0) shininess)
                       0.0))
         (color (+ (* ambient-color albedo)
                   (* (* light-color albedo) diffuse)
                   (* light-color specular))))
    (v! color 1.0)))

(defpipeline-g render-surface ()
  (surface-vert g-pn)
  (surface-frag :vec3 :vec3))
