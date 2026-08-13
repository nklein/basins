;;; drawing.lisp

(in-package :basins)

;;;; ------------------------------------------------------------------
;;;; Drawing
;;;; ------------------------------------------------------------------

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
    (let* ((r (ball-radius ball))
           (model->world (m4:* (m4:translation (ball-position ball))
                               (m4:scale (v! r r r)))))
      (map-g #'render-surface *ball-stream*
             :model->world model->world
             :normal-matrix (normal-matrix model->world)
             :albedo (ball-color ball)
             :shininess *ball-shininess*))))

(defun draw-frame ()
  (clear)
  ;; Uniforms that hold for the whole frame are set once against a NIL stream;
  ;; CEPL keeps them until they are next changed.
  (map-g #'render-surface nil
         :world->cam (world->cam)
         :cam->clip (cam->clip)
         :eye-position *camera-position*
         :light-direction *light-direction*
         :light-color *light-color*
         :ambient-color *ambient-color*)
  (draw-field)
  (map nil #'draw-ball *balls*)
  (swap))
