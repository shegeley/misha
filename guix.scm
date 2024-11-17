(use-modules
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
  (native-inputs (list sbcl-wayflan/latest))))

hatis
