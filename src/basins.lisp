;;;; basins.lisp

(in-package #:basins)

;;;; ------------------------------------------------------------------
;;;; Main loop
;;;; ------------------------------------------------------------------

(defun step-frame ()
  (step-host)                           ; pump the host's event queue
  (update-repl-link)                    ; keep the REPL alive while running
  (let* ((now (get-internal-real-time))
         (dt (if *last-frame-time*
                 (min (float (/ (- now *last-frame-time*)
                                internal-time-units-per-second)
                             1.0)
                      *max-timestep*)
                 0.0)))
    (setf *last-frame-time* now)
    (update-camera-from-mouse)          ; after STEP-HOST, so events are in
    (update-balls dt)
    (draw-frame)))

(defun start (&optional (width 640) (height 480))
  "Create the window and GL context.  Call once per session, from the REPL."
  (cepl:repl width height))

(defun run ()
  (init)
  (sync-viewport)
  (setf *running* t)
  (whilst-listening-to ((#'resize-viewport (window 0) :size))
    (loop :while (and *running* (not (shutting-down-p)))
          :do (continuable (step-frame))))
  (values))

(defun stop ()
  (setf *running* nil))
