(define-module (hatis packages sbcl-input-event-codes)
 #:use-module (gnu packages)
 #:use-module (guix gexp)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix git-download)
 #:use-module (guix utils)
 #:use-module (guix build-system asdf)
 #:use-module (gnu packages pkg-config)
 #:use-module (gnu packages lisp-check)
 #:use-module (gnu packages lisp-xyz))

(define-public sbcl-input-event-codes
 (let ((commit "9804fe1e8fcfaab51097ea30e1045b7cc5e0ef3e")
       (revision "1"))
  (package
   (name "sbcl-input-event-codes")
   (version (git-version "0.0.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~shunter/input-event-codes")
           (commit commit)))
     (file-name (git-file-name "sbcl-input-event-codes" version))
     (sha256
      (base32 "1m96m9ia4frcn2xqaw4mfspjjzwl8gyj4k4rv0lq28va4s6mkgii"))))
   (build-system asdf-build-system/sbcl)
   (native-inputs (list pkg-config sbcl-parachute))
   (arguments
    (list
     #:asd-systems ''("input-event-codes" "input-event-codes/test")))
   (inputs (list sbcl-trivial-features))
   (home-page "https://git.sr.ht/~shunter/input-event-codes")
   (synopsis
    "This little library is a port of all constants found in input-event-codes.h, an event code header file found on both Linux and FreeBSD")
   (description
    "There is currently only support for Linux and FreeBSD. The library uses trivial-features to conditionally load the appropriate constant set.")
   (license license:expat))))
