(defpackage :xyz.hatis.core
 (:use
  :wayflan-client

  :xyz.hatis.protocols.data-control
  :xyz.hatis.protocols.input-method
  :xyz.hatis.protocols.foreign-toplevel-management

  :cl)
 (:local-nicknames (#:a #:alexandria)))

(in-package #:xyz.hatis.core)

(defparameter foreign-toplevel-manager nil)
(defparameter foreign-toplevel-handle  nil)
(defparameter registry nil)

(defun handle-toplevel-handle (toplevel-handle)
 (push (lambda (event)
        (destructuring-bind (name &rest args) event
         (finish-output nil)
         (format t "Proxy received event ~S with args ~S~%" event args)
         (finish-output nil)))
  (wl-proxy-hooks toplevel-handle)))

(defun handle-toplevel-manager (display toplevel-manager)
 (push (lambda (event)
        (destructuring-bind (name &rest args) event
         (finish-output nil)
         (format t "Proxy received event ~S with args ~S~%" event args)
         (finish-output nil)
         (setf foreign-toplevel-handle (first args))
         (handle-toplevel-handle foreign-toplevel-handle)))
  (wl-proxy-hooks toplevel-manager)))


(defun handle-registry (display)
 (setf registry (wl-display.get-registry display))
 (push
  (lambda (event)
   (destructuring-bind (name &optional id interface version) event
    (when (string= interface "zwlr_foreign_toplevel_manager_v1")
     (setf foreign-toplevel-manager
      (wl-registry.bind registry id 'zwlr-foreign-toplevel-manager-v1 version))
     (handle-toplevel-manager display foreign-toplevel-manager))))
  (wl-proxy-hooks registry))
 (wl-display-roundtrip display)
 (wl-display-roundtrip display))

(defun run ()
 (with-open-display (display)
  (handle-registry display)
  (loop (wl-display-dispatch-event display))))

(run)
