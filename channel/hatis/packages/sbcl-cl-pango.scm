(define-module (hatis packages sbcl-cl-pango)
 #:use-module (gnu packages)
 #:use-module (guix gexp)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (guix packages)
 #:use-module (guix git-download)
 #:use-module (guix utils)
 #:use-module (guix build-system asdf)
 #:use-module (gnu packages pkg-config)
 #:use-module (gnu packages gtk)
 #:use-module (gnu packages lisp-xyz))

(define-public sbcl-cl-pango
 (let ((commit "ee4904d19ce22d00eb2fe17a4fe42e5df8ac8701")
       (revision "1"))
  (package
   (name "sbcl-cl-pango")
   (version revision)
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/BradWBeer/cl-pango")
           (commit commit)))
     (file-name (git-file-name "cl-pango" version))
     (sha256
      (base32 "0zkn4yn8nkkjr0x1vcy856cvbmnyhdidqz0in8xvd2i93jvw5w0i"))))
   (build-system asdf-build-system/sbcl)
   (arguments
    (list
     #:asd-systems ''("cl-pango")
     #:phases #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-paths
                  (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "library.lisp"
                    (("libpango-[0-9.]*\\.so" all)
                     (string-append #$(this-package-input "pango")
                      "/lib/libpango-1.0.so.0"))
                    (("libpangocairo-[0-9.]*\\.so" all)
                     (string-append #$(this-package-input "pango")
                      "/lib/libpangocairo-1.0.so.0"))))))))
   (inputs (list sbcl-cffi sbcl-cl-cairo2 sbcl-xmls pango cairo))
   (home-page "https://github.com/BradWBeer/cl-pango")
   (synopsis "Pango bindings for Common Lisp")
   (description "Bindings to the pango text layout library")
   (license license:expat))))
