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

(defun get-interface* (type)      (state-ref type))
(defun set-interface* (interface) (state-set (type-of interface) interface))

(defun interface-alive-p (type)
 (not (eql 'wl-destroyed-proxy (type-of (get-interface* type)))))

(defun set-interface (interface)
 (let ((type (type-of interface)))
  (if (interface-alive-p type) (set-interface* interface) nil)))

(defun get-interface (type)
 (let ((I (get-interface* type)))
  (if (interface-alive-p type) I nil)))

(defvar registry-global-interfaces-bind-list
 ;; "List of 'initial' (:= coming from the registry) interfaces that's needed by hatis"
 `(wl-seat
   zwp-input-method-manager-v2
   zwlr-foreign-toplevel-manager-v1))

(defmethod handle-interface-event (i e) (lambda (&rest args) args))

(defmethod handle-interface-event
 ((i zwlr-foreign-toplevel-manager-v1) (e (eql :toplevel)))
 (lambda (handle) (process-interface handle)))

(defmethod handle-interface-event
 ((i zwp-input-method-v2) (e (eql :activate)))
 (lambda ()
  (let ((grab (zwp-input-method-v2.grab-keyboard i)))
   (process-interface grab))))

(defmethod handle-interface-event
 ((i zwp-input-method-v2) (e (eql :deactivate)))
 (lambda ()
  (let ((grab (get-interface 'zwp-input-method-keyboard-grab-v2)))
   (when grab
    (zwp-input-method-keyboard-grab-v2.release grab)
    (list 'released 'zwp-input-method-keyboard-grab-v2)))))

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
 "Handle wayland's interface event according to the handle-interface-event method and send result to the channel"
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
  (set-interface interface)
  (install-event-handlers! interface)
  (list 'processed interface)))

(defun get-input-method ()
 (let* ((imm  (get-interface 'zwp-input-method-manager-v2))
        (im   (get-interface 'zwp-input-method-v2))
        (seat (get-interface 'wl-seat)))
  (if im im (zwp-input-method-manager-v2.get-input-method imm seat))))

(defun run ()
 (with-open-display (display)
  (process-interface display) ;; <- this will "catch" all the interfaces into *state* hashtable + put event-listeners on them
  (process-interface (get-input-method)) ;; <- this will 'catch' via input-method-manager-v2.get-input-method. need to be evoked once *state* if fully filled
  (loop (wl-display-dispatch-event display))))

;; (pexec () (loop (format t "~a~%" (recv *channel*))))
;; (run)
