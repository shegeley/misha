;; repl.lisp
(require :asdf)

(asdf:require-system :slynk)

(slynk:create-server :port 4006 :dont-close t)
