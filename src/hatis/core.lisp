(defpackage :xyz.hatis.core
 (:use
  :wayflan-client

  :xyz.hatis.protocols.data-control
  :xyz.hatis.protocols.input-method
  :xyz.hatis.protocols.foreign-toplevel-management

  :xyz.hatis.utils

  :chanl
  :access
  :arrows
  :cl)
 (:local-nicknames
  (:a :alexandria)
  (:t :trivia)))

(in-package :xyz.hatis.core)

(defvar *channel* (make-instance 'channel))
(defvar *state*   (make-hash-table))

(defun state-ref (key)     (gethash key *state*))
(defun state-set (key val)
 (setf (gethash key *state*) val)
 (list 'state-set key val))
#| In CL I can't just replace [1] with `(state-ref)'. setf won't recognize. it's ugly af. maybe: use `access' lib for it? But it's an overkill for 1 deep hash-table. Maybe for another one.
(setf (access:accesses *heroes* "Avengers" "Retired" "Tony Stark") "me") https://stackoverflow.com/a/56596316 |#
(defun state-unset (key)   (remhash key *state*))

(defvar registry-global-interfaces-bind-list
 `(wl-seat
   zwp-input-method-manager-v2
   zwlr-foreign-toplevel-manager-v1))

(defmethod handle-interface-event (i e) (lambda (&rest args) args))

(defmethod handle-interface-event
 ((i zwlr-foreign-toplevel-manager-v1) (e (eql :toplevel)))
 (lambda (handle) (state-set 'foreign-toplevel-handle handle)))

(defmethod handle-interface-event
 ((registry wl-registry) (e (eql :global)))
 (lambda (id interface version)
  (let ((sinterface (interface-string->symbol interface)))
   (if (find sinterface registry-global-interfaces-bind-list)
    (let ((interface-object (bind registry id interface version)))
     (process-interface interface-object))
    (list 'wl-registry 'global (list id interface version))))))

(defun handle-interface-event*
 (interface event &key (channel *channel*))
 "Handle wayland's interface event accoding to the handle-interface-event method and send result to the channel"
 (destructuring-bind (event-name &rest event-args) event
  (let ((r (apply (handle-interface-event interface event-name) event-args)))
   (send channel r))))

(defun install-event-handlers! (interface)
 (push
  (lambda (event) (handle-interface-event* interface event))
  (wl-proxy-hooks interface)))

(defun bind (registry id interface version)
 (wl-registry.bind registry id (interface-string->symbol interface) version))

(defmethod process-interface
 ((display wl-display))
 (progn
  (process-interface (wl-display.get-registry display))
  (wl-display-roundtrip display)
  (wl-display-roundtrip display)))

(defmethod process-interface
 (interface)
 "This method is called BEFORE all the interfaces are 'collected' into *state* hashtable. So you can't rely on it's being filled on this method's first call"
 (progn
  (state-set (type-of interface) interface)
  (install-event-handlers! interface)
  (list 'processed interface)))

(defun get-input-method ()
 (let* ((imm  (state-ref 'zwp-input-method-manager-v2))
        (im   (state-ref 'zwp-input-method-v2))
        (seat (state-ref 'wl-seat))
        (im*  (if im im (zwp-input-method-manager-v2.get-input-method imm seat))))
  (process-interface im*)))

(defun run ()
 (with-open-display (display)
  (process-interface display)
  (get-input-method)
  (loop (wl-display-dispatch-event display))))

;; (pexec () (loop (format t "~a~%" (recv *channel*))))
;; (run)
