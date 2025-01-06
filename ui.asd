(defsystem :hatis.ui
 :description "UI for the Hackable Text Input System"
 :version "0.0.1"
 :license "GPL-v3"
 :author "Grigory Shepelev"

 :homepage "https://github.com/shegeley/hatis"
 :source-control (:git "https://github.com/shegeley/hatis")

 :pathname #P"src/"

 :defsystem-depends-on (:hatis
                        :trivial-features ;; base
                        :alexandria ;; base
                        :trivia ;; matching
                        ;; gtk
                        :cl-gtk4
                        :cl-glib
                        ;; -> ->>
                        :arrows
                        ;; CSP multhithreading
                        :chanl
                        ;; nice way to access nested structs elements
                        :access)

 :components ((:file "hatis/utils")
              (:file "hatis/ui/gtk" (:depends-on "hatis/utils"))))
