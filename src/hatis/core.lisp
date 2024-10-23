(defpackage :hatis.core
  (:use #:cl #:wayflan-client))

(defun run ()
  ;; Try to connect to a server socket at $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY.
  ;; If $WAYLAND_DISPLAY describes an absolute path, connect to that directly.
  (with-open-display (display)
    ;; Create a registry to provide a list of all
    ;; globals the client can bind to.
    (let ((registry (wl-display.get-registry display)))
      ;; Push an event-listening closure to a list that is called
      ;; whenever the registry receives an event.
      (push (lambda (event-name &rest event-args)
              ;; The macro EVCASE dispatches based on the event automatically.
              ;; See examples/hello-world.lisp
              (when (eq event-name :global)
                (destructuring-bind (name interface version) event-args
                  ;; Print all globals, their interface names, and latest
                  ;; supported version
                  (format t "#x~8,'0X ~32S v~D~%"
                          name interface version))))
            (wl-proxy-hooks registry))

      ;; Listen until all wl-registry events are processed
      (format t "wl-registry globals:~%")
      (wl-display-roundtrip display))))
