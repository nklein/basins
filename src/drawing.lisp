;;; drawing.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Drawing
;;;; ------------------------------------------------------------------

(defun ball-model->world (ball)
  "Where BALL's unit sphere sits in the world.
Shared by the beauty pass and the depth pass so the two cannot disagree."
  (let ((r (radius ball)))
    (m4:* (m4:translation (pos ball))
          (m4:scale (v! r r r)))))

(defun draw-field ()
  (when *field-stream*
    (let ((model->world (m4:identity)))
      (map-g #'render-surface *field-stream*
             :model->world model->world
             :normal-matrix (normal-matrix model->world)
             :albedo *field-albedo*
             :shininess *field-shininess*))))

(defun draw-ball (ball)
  (when *ball-stream*
    (let ((model->world (ball-model->world ball)))
      (map-g #'render-surface *ball-stream*
             :model->world model->world
             :normal-matrix (normal-matrix model->world)
             :albedo (color ball)
             :shininess *ball-shininess*))))

(defun draw-depth-pass ()
  "Record, from the light's viewpoint, the distance to the nearest surface.

Everything that can cast has to be drawn here, and only the depth buffer is
kept.  WITH-FBO-BOUND retargets the viewport to the map's size for the
duration and restores it afterwards, so the window's viewport is unaffected."
  (when (and *shadow-fbo* *field-stream* *ball-stream*)
    ;; :ATTACHMENT-FOR-SIZE must name the depth attachment, as there is no
    ;; colour attachment 0 to take the size from.  Draw buffers are left to
    ;; default: WITH-FBO-BOUND then issues glDrawBuffers(0) and records that
    ;; in the context, which is both correct for a colour-less FBO and what
    ;; keeps CLEAR's own save/restore of that state consistent.
    (with-fbo-bound (*shadow-fbo* :attachment-for-size :d :with-blending nil)
      ;; Clear what is bound rather than naming the FBO: the no-argument form
      ;; is a bare glClear, and does not touch draw-buffer state at all.
      (clear)
      (map-g #'render-depth nil :world->light-clip (world->light-clip))
      (map-g #'render-depth *field-stream* :model->world (m4:identity))
      (dolist (ball *balls*)
        (map-g #'render-depth *ball-stream*
               :model->world (ball-model->world ball))))))

(defun draw-frame ()
  (draw-depth-pass)
  (clear)
  ;; Uniforms that hold for the whole frame are set once against a NIL stream;
  ;; CEPL keeps them until they are next changed.
  (map-g #'render-surface nil
         :world->cam (world->cam)
         :cam->clip (cam->clip)
         :eye-position *camera-position*
         :light-direction *light-direction*
         :light-color *light-color*
         :ambient-color *ambient-color*
         :world->light-clip (world->light-clip)
         :shadow-map *shadow-sampler*
         :shadow-bias-min *shadow-bias-min*
         :shadow-bias-max *shadow-bias-max*
         ;; Texel size scaled by the softness, so both can be tuned live.
         :shadow-step (/ *shadow-softness* *shadow-map-size*))
  (draw-field)
  (map nil #'draw-ball *balls*)
  (swap))
