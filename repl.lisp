;; repl.lisp
(require :asdf)

(asdf/operate:require-system :slynk)

(asdf:load-asd (merge-pathnames "hatis.asd" (uiop:getcwd)))

(slynk:create-server :port 4005 :dont-close t)
