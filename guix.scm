(use-modules
 (hatis packages sbcl-wayflan)

 (gnu packages)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix packages)
 (guix git-download)
 (guix utils)
 (guix build-system asdf)

 (gnu packages freedesktop)
 (gnu packages pkg-config)
 (gnu packages xdisorg)
 (gnu packages lisp-check)
 (gnu packages lisp-xyz))

(define %source-dir (dirname (current-filename)))

(define-public hatis
 (package
  (name "hatis")
  (home-page "https://github.com/shegeley/hatis")
  (description "This is a very early-stage project (alpha-version) + a set of experiments of building HAckable Text Input System (HATIS)")
  (synopsis "")
  (license license:gpl3+)
  (source (local-file %source-dir "text-input-system-checkout"
           #:recursive? #t))
  (build-system asdf-build-system/sbcl)
  (arguments (list
              #:tests? #f ;; looks for hatis/test
              #:asd-systems ''("hatis")))
  (version "0.0.1-alpha")
  (inputs (list sbcl-alexandria))
  (native-inputs (list sbcl-wayflan))))

hatis
