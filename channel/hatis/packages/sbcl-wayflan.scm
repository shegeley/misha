(define-module (hatis packages sbcl-wayflan)
 #:use-module (gnu packages)
 #:use-module (guix gexp)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix git-download)
 #:use-module (guix utils)
 #:use-module (guix build-system asdf)

 #:use-module (hatis packages sbcl-xkb)
 #:use-module (hatis packages sbcl-cl-pango)
 #:use-module (hatis packages sbcl-input-event-codes)
 #:use-module (hatis packages sbcl-posix-shm)

 #:use-module (gnu packages freedesktop)
 #:use-module (gnu packages pkg-config)
 #:use-module (gnu packages xdisorg)
 #:use-module (gnu packages lisp-check)
 #:use-module (gnu packages lisp-xyz))

(define-public sbcl-wayflan
 (let ((commit "3fbd2164e96aece3993929c7076593a2ee594f50")
       (revision "1"))
  (package
   (name "sbcl-wayflan")
   (version (git-version "0.0.4" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~shunter/wayflan")
           (commit commit)))
     (file-name (git-file-name "wayflan" version))
     (sha256
      (base32 "0y6hzskp1vgaigzj5b3i695sc6dn5mk7nlxs21nh5ybzmf4chhyy"))))
   (build-system asdf-build-system/sbcl)
   (arguments
    (list
     #:tests? #f ;tries to look for wayflan-client/test
     #:asd-systems ''("wayflan-client"
                      ;; "wayflan-client/examples" fails on wayland-keyboard-demo.lisp because xkb used as :xkb, not required properly
                      "wayflan" "wayflan/common" "wayflan/test")))
   (inputs (list
            sbcl-cffi
            sbcl-alexandria
            sbcl-babel
            sbcl-plump
            sbcl-cl-colors
            sbcl-cl-cairo2
            sbcl-closer-mop

            sbcl-input-event-codes
            sbcl-xkb
            sbcl-cl-pango
            sbcl-posix-shm

            wayland))
   (native-inputs (list sbcl-parachute))
   (home-page "https://git.sr.ht/~shunter/wayflan")
   (synopsis
    "Wayflan is a from-scratch Wayland communication library for Common Lisp. It is not a binding or a wrapper around libwayland, but a re-implementation of the Wayland protocol. This unties Lisp applications from per-proxy manual memory allocation, toplevel-only C callbacks, and enables a closer interface with lisp")
   (description
    "Wayflan makes a good-faith effort to mimic @code{libwayland} behavior not defined in the Wayland spec, to keep compatibility between the two libraries.

Wayflan is not a compositor nor a GUI toolkit. Its purpose is to parse Wayland protocol XML documents and exchange Wayland messages between other processes.

Wayflan is an ongoing project. Things may change to make Wayflan the best client for Common Lisp projects, such as iterating on a better API. If you use Wayflan in any projects, let me know! I'd love to give a heads up and help transition projects to any breaking changes I make along the way.

Features:
@itemize
@item Client support
@item All implementation done in Common Lisp from the socket up
@item Enum values are translated into keywords
@item Wayland protocol introspection
@item ASDF component @code{:wayflan-client-impl} generates code from XML. ASDF's extensible components make it possible to teach your program new protocols for Wayland without the need of a special build system.
@end itemize")
   (license license:bsd-3))))
