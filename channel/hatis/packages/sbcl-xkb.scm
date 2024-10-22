(define-module (hatis packages sbcl-xkb)
 #:use-module (gnu packages)
 #:use-module (guix gexp)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix git-download)
 #:use-module (guix utils)
 #:use-module (guix build-system asdf)
 #:use-module (gnu packages pkg-config)
 #:use-module (gnu packages xdisorg)
 #:use-module (gnu packages lisp-xyz))

(define-public sbcl-xkb
 (let ((commit "e69c0e85e59fd8271efa2e9659cfffa9b59d1ece")
       (revision "0"))
  (package
   (name "sbcl-xkb")
   (version (git-version "0.0.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/malcolmstill/cl-xkb")
           (commit commit)))
     (file-name (git-file-name "cl-xkb" version))
     (sha256
      (base32 "01k4hf316xr2h149vf7sh45rrdkfppqnzakzn91i7lw46dwl57wh"))))
   (build-system asdf-build-system/sbcl)
   (native-inputs (list pkg-config))
   (arguments
    (list
     #:asd-systems ''("cl-xkb")
     #:phases #~(modify-phases %standard-phases
                 (add-after 'unpack 'add-xkbcommon
                  (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "cl-xkb.lisp"
                    (("/usr/lib64/libxkbcommon.so.0")
                     (string-append #$(this-package-input "libxkbcommon")
                      "/lib/libxkbcommon.so"))))))))
   (inputs (list libxkbcommon sbcl-cffi))
   (home-page "https://github.com/malcolmstill/cl-xkb")
   (synopsis
    "@code{cl-xkb} is a Common Lisp wrapper for @code{libxkbcommon}")
   (description
    "@code{cl-xkb} started development to support @url{https://github.com/malcolmstill/ulubis, ulubis}

The library currently supports these xkb modules:

@itemize
@item Keysyms
@item Library Context
@item Include Paths
@item Logging Handling
@item Keymap Creation
@item Keymap Components
@item Keyboard State
@item Compose and dead-keys support
@end itemize")
   (license license:bsd-3))))
