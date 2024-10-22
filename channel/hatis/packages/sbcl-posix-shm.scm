(define-module (hatis packages sbcl-posix-shm)
 #:use-module (gnu packages)
 #:use-module (guix gexp)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix git-download)
 #:use-module (guix utils)
 #:use-module (guix build-system asdf)
 #:use-module (gnu packages lisp-check)
 #:use-module (gnu packages lisp-xyz))

(define-public sbcl-posix-shm
 (let ((commit "9443da23bcda0a2bd727aed675f92ee7d3c51802")
       (revision "0.0.7"))
  (package
   (name "sbcl-posix-shm")
   (version (git-version "0.0.0" revision commit))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~shunter/posix-shm")
           (commit commit)))
     (file-name (git-file-name "sbcl-posix-shm" version))
     (sha256
      (base32 "0ah7xh7dxvdk58slic60gx7k56idjw5x30q5ifg90hxfhd32qz6l"))))
   (build-system asdf-build-system/sbcl)
   (arguments
    (list
     #:asd-systems ''("posix-shm" "posix-shm/ffi" "posix-shm/test")))
   (inputs (list sbcl-cffi sbcl-alexandria sbcl-trivial-features))
   (native-inputs (list sbcl-osicat sbcl-parachute))
   (home-page "https://git.sr.ht/~shunter/posix-shm")
   (synopsis
    "Common Lisp bindings and wrapper for the POSIX shared memory API")
   (description
    "This library provides two strata to access the POSIX shm API:

@itemize
@item The package @code{posix-shm/ffi}, a collection of slim bindings to the POSIX API
@item The package @code{posix-shm}, a lispy wrapper around the FFI that integrates more closely to the features of Common Lisp, and provides a handful of utilities and macros
@end itemize

Features include:

@itemize
@item open, close, create, resize, change ownership of, change permissions of, and memory map to shared memory objects
@item @code{open-shm} appears more like @code{open} from the standard library
@item @code{open-shm*}, for creating anonymous shm objects
@item @code{with-open-shm}, @code{with-mmap} and similar @code{with-} macros for safely accessing resources with dynamic extent
@end itemize")
   (license license:bsd-3))))
