;; repl.lisp
(require :asdf)
(require :slynk)
(require :uiop)

(asdf:load-asd (merge-pathnames "hatis.asd" (uiop:getcwd)))
(asdf:load-system :hatis)

(slynk:create-server :port 4005 :dont-close t)
