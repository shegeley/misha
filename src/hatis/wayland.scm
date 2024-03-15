(define-module (hatis wayland)
  #:use-module (wayland proxy)
  #:use-module (wayland interface)
  #:use-module (wayland client display)
  #:use-module (wayland client protocol input-method)
  #:use-module (wayland client protocol wayland)
  #:use-module (wayland client protocol xdg-shell)

  #:use-module (hatis wayland keyboard)

  #:use-module (fibers)
  #:use-module (fibers channels)

  #:use-module (srfi srfi-1)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)

  #:use-module (oop goops))

(define (current-desktop)
  (getenv "XDG_CURRENT_DESKTOP"))

(define chan
  (make-channel))

(define compositor
  (make-parameter #f))

(define display
  (make-parameter #f))

(define registry
  (make-parameter #f))

(define seat
  (make-parameter #f))

(define input-method-manager
  (make-parameter #f))

(define input-method
  (make-parameter #f))

(define input-surface
  ;; https://wayland.app/protocols/input-method-unstable-v2#zwp_input_popup_surface_v2
  (make-parameter #f))

(define xdg-input-surface
  ;; https://wayland.app/protocols/xdg-shell#xdg_surface
  (make-parameter #f))

(define keyboard
  (make-parameter #f))

(define xdg-wm-base
  ;; The xdg_wm_base interface is exposed as a global object enabling clients to turn their wl_surfaces into windows in a desktop environment. It defines the basic functionality needed for clients and the compositor to create windows that can be dragged, resized, maximized, etc, as well as creating transient windows such as popup menus.
(make-parameter #f))

(define registry-listener
  (make <wl-registry-listener>
    #:global
    (lambda* (data registry name interface version)
      (format #t "interface: '~a', version: ~a, name: ~a~%"
              interface version name)
      (cond
       ((string=? "wl_compositor" interface)
        (compositor
         (wrap-wl-compositor (wl-registry-bind registry name %wl-compositor-interface 3))))
       ((string=? "wl_seat" interface)
        (seat (wrap-wl-seat (wl-registry-bind registry name %wl-seat-interface 3))))
       ((string=? "zwp_input_method_manager_v2" interface)
        (input-method-manager
         (wrap-zwp-input-method-manager-v2
          (wl-registry-bind registry name %zwp-input-method-manager-v2-interface 1))))
       ((string=? "xdg_wm_base" interface)
        (xdg-wm-base
         (wrap-xdg-wm-base
          (wl-registry-bind registry name %xdg-wm-base-interface 2))))))
    #:global-remove
    (lambda (data registry name)
      (pk 'remove data registry name))))

(define (handle-key-press . args)
  "let if be as is for now. but I guess enhanced interception logic needed.
   like:
    (define (wrap-handle-press-event pointer grab serial timestamp key state)
        (alist->hash-table `((serial . ,serial)
                            (timestamp . ,timestamp)
                            (key . ,key)
                            (state . ,state))))"
  (format #t "key! args: ~a ~%" args)
  (put-message chan (list #:key args)))

(define keyboard-grab-listener
  (make <zwp-input-method-keyboard-grab-v2-listener>
    #:release
    (lambda args
      (format #t "release! args: ~a ~%" args))
    #:keymap
    (lambda args
      (format #t "keymap! args: ~a ~%" args)
      (apply get-keymap (drop args 2)))
    #:modifiers
    (lambda args
      (format #t "modifiers! args: ~a ~%" args))
    #:repeat-info
    (lambda args
      (format #t "repeat-info! args: ~a ~%" args))
    #:key handle-key-press))

(define input-method-listener
  (make <zwp-input-method-v2-listener>
    #:text-change-cause
    (lambda args
      (format #t "cause! args: ~a ~%" args))
    #:content-type
    (lambda args
      (format #t "content-type! args: ~a ~%" args))
    #:surrounding-text
    (lambda args
      (format #t "surrounding! args: ~a ~%" args))
    #:unavailable
    (lambda args
      (format #t "unavailable! args: ~a ~%" args))
    #:done
    (lambda args
      (format #t "done! args: ~a ~%" args))
    #:commit-string
    (lambda args
      (format #t "commit! args: ~a ~%" args))
    #:activate
    (lambda (_ im)
      (format #t "activate! im: ~a ~%" im)
      ;; NOTE: need to grab keyboard + input surface

      ;; Catch surface
      (input-surface
       (wl-compositor-create-surface (compositor)))

      (input-surface
       ;; popup-input-surface won't cast to xdg-surface
       ;; (xdg-input-surface (xdg-wm-base-get-xdg-surface (xdg-wm-base) (input-surface)))
       ;; ERROR: «not a <wl-surface> or #f #<<zwp-input-popup-surface-v2> 7efd12918c80>»
       (zwp-input-method-v2-get-input-popup-surface im (input-surface)))

      ;; Grab keyboard
      (keyboard (zwp-input-method-v2-grab-keyboard im))
      (zwp-input-method-keyboard-grab-v2-add-listener (keyboard) keyboard-grab-listener))
    #:deactivate
    (lambda args
      (format #t "leave! args: ~a ~%" args)
      ;; Release keyboard NEEDED?
      ;; (zwp-input-method-keyboard-grab-v2-release (keyboard))
      )))

(define (main)
  (display (wl-display-connect))
  (unless (display)
    (display "Unable to connect to wayland compositor")
    (newline)
    (exit -1))
  (format #t "Connect to Wayland compositor: ~a ~%" (display))
  (registry (wl-display-get-registry (display)))
  (wl-registry-add-listener (registry) registry-listener)
  (wl-display-roundtrip (display))
  (if (input-method-manager)
      (format #t "Got it!~%")
      (error (format #f "Can't access input-manager!")))
  (format #t "Input-method manager: ~a ~%" (input-method-manager))
  (input-method (zwp-input-method-manager-v2-get-input-method
                 (input-method-manager)
                 (seat)))
  (format #t "Input-method: ~a ~%" (input-method))
  (zwp-input-method-v2-add-listener (input-method) input-method-listener)
  (while (wl-display-roundtrip (display))))

;; Evaluate (main) only AFTER evaluating everything before it. Or ~arei~ buffer might loose the (current-output-(port?)) and won't show any print commands
;; (main)

;; (use-modules (ice-9 textual-ports))
;; TODO: parameters => (ice-9 atomics)?

(define thread
  (call-with-new-thread
   (lambda ()
     (with-output-to-file "./output.txt"
       (lambda () (main))))))

;; (cancel-thread thread) <- stop

;; use system-async-mark on the thread (need to create continuation first) <- pause
