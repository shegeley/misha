(defpackage :xyz.hatis.core
 (:use
  :wayflan-client
  :xyz.shunter.wayflan.client.scanner
  :xyz.hatis.interfaces.foreign-toplevel-manager
  :cl)
 (:local-nicknames (#:a #:alexandria)))

(in-package #:xyz.hatis.core)

(defparameter foreign-toplevel-manager nil)
(defparameter foreign-toplevel-handle  nil)
(defparameter registry nil)

(defun handle-toplevel-manager (toplevel-manager)
 (push (evlambda
        (t (name &rest args)
         (format t "toplevel-manager event: ~S ~S~%" name args)))
  (wl-proxy-hooks toplevel-manager)))

(defun handle-toplevel-handle (toplevel-handle)
 (push (evlambda
        (t (name &rest args)
         (format t "toplevel event: ~S ~S~%" name args)))
  (wl-proxy-hooks toplevel-handle)))

(defun handle-registry (display)
 (let ((registry (wl-display.get-registry display)))
  (push
   (lambda (event-name &rest event-args)
    (when (eq event-name :global)
     (destructuring-bind (name interface version) event-args
      (when (string= interface "zwlr_foreign_toplevel_manager_v1")
       (let ((foreign-toplevel-manager
              (wl-registry.bind
               registry name
               'zwlr-foreign-toplevel-manager-v1 version)))
        (handle-toplevel-manager foreign-toplevel-manager))))))
   (wl-proxy-hooks registry))
  (wl-display-roundtrip display)))

(defun run ()
 (with-open-display (display)
  (handle-registry display)
  (loop (wl-display-dispatch-event display))))

(run)
