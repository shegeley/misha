;; repl.lisp
(require :asdf)

(asdf:require-system :slynk)

(asdf:load-asd (merge-pathnames "hatis.asd" (uiop:getcwd)))

(slynk:create-server :port 4005 :dont-close t)
