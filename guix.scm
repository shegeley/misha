(use-modules
 (gnu packages)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix packages)
 (guix git-download)
 (guix utils)
 (guix build-system asdf)

 (hatis packages wlroots)

 ((gnu packages glib) #:select (glib gobject-introspection))
 ((gnu packages gtk) #:select (gtk))
 (gnu packages freedesktop)
 (gnu packages pkg-config)
 (gnu packages xdisorg)
 (gnu packages lisp-check)
 (gnu packages lisp-xyz))

(define %protocols
 (local-file
  (dirname (current-filename))
  "hatis-protocols"
  #:recursive? #t
  ;; guix will auto-compile all the .lisp file and pull all the .asd files into knows systems automatically on build with asdf-build-system
  ;; unrelated files must be removed from the source of the desired packages to avoid collisions
  #:select?
  (lambda (file _)
   (and
    (not (string-contains file "hatis.asd"))
    (not (string-contains file "repl.lisp"))
    (not (string-contains file "/src/"))))))

(define %source
 (local-file
  (dirname (current-filename))
  "hatis"
  #:recursive? #t
  #:select?
  (lambda (file _)
   (and
    (not (string-contains file "repl.lisp"))
    (not (string-contains file "protocols.asd"))
    (not (string-contains file "/protocols/"))))))

(define sbcl-wayflan/latest
 (let [(commit "f56f6ec42b05100ef7353a831b9f9ad505824c95")
       (hash "1l09jzji0c70xh8pv2fn0cqbrayhnfgf4bqps3s846b3dw2hzg2v")]
  (package
   (inherit sbcl-wayflan)
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~shunter/wayflan")
           (commit commit)))
     (sha256 (base32 hash)))))))

(define protocols/phases
 #~(modify-phases %standard-phases
    (add-after 'unpack 'fix-paths
     (lambda* (#:key inputs #:allow-other-keys)
      (substitute* (find-files "protocols")
       (("\"(.*?.xml)\"" all m)
        (let* ((rel-path (string-append "/share/wayland-protocols/" m))
               (abs-path (search-input-file inputs rel-path)))
         (string-append "#P\"" abs-path "\""))))))))

(define-public hatis/protocols
 (package
  (name "hatis-protocols")
  (home-page "https://github.com/shegeley/hatis")
  (description "Wayflan-Spiced Wayland Protocols Needed For Hatis")
  (synopsis "")
  (license license:gpl3+)
  (source %protocols)
  (build-system asdf-build-system/sbcl)
  (arguments
   (list
    #:tests? #f ;; looks for hatis/protocols/test
    #:phases protocols/phases
    #:asd-systems ''("protocols")))
  (version "0.0.1")
  (native-inputs (list sbcl-wayflan/latest wlroots* wayland-protocols))))

(define-public hatis
 (package
  (name "hatis")
  (home-page "https://github.com/shegeley/hatis")
  (description "This is a very early-stage project (alpha-version) + a set of experiments of building HAckable Text Input System (HATIS)")
  (synopsis "")
  (license license:gpl3+)
  (source %source)
  (build-system asdf-build-system/sbcl)
  (arguments
   (list
    #:tests? #f ;; looks for hatis/test
    #:asd-systems ''("hatis")))
  (version "0.0.1-alpha")
  (inputs
   (list
    ;; sbcl-modf ;; todo: take a look
    sbcl-wayflan/latest
    hatis/protocols
    sbcl-chanl
    sbcl-trivia
    sbcl-access
    sbcl-arrows
    sbcl-alexandria))))

(define-public hatis.ui
 (package
  (inherit hatis)
  (name "hatis-ui")
  (arguments (list #:tests? #f #:asd-systems ''("hatis.ui")))
  (inputs
   (append (package-inputs hatis)
    (list
     gobject-introspection
     sbcl-cl-gtk4
     sbcl-cl-glib
     gtk
     glib)))))

hatis
