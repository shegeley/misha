(defsystem :hatis
 :description "Hackable Text Input System (provided via Wayland Protocols)"
 :version "0.0.1"
 :license "GPL-v3"
 :author "Grigory Shepelev"

 :homepage "https://github.com/shegeley/hatis"
 :source-control (:git "https://github.com/shegeley/hatis")

 :pathname #P"src/"

 :defsystem-depends-on (:trivial-features ;; base
                        :alexandria ;; base
                        :trivia ;; matching
                        :arrows ;; syntax
                        :chanl ;; CSP multhithreading
                        :access ;; nice way to access nested structs elements
                        :wayflan ;; best lisp wayland client
                        :protocols)

 :components ((:module "hatis"
               :components ((:file "utils")
                            (:file "core" :depends-on ("utils"))))))
