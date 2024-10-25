(defsystem #:hatis
 :description "Hackable Text Input System (provided via Wayland Protocols)"
 :version "0.0.1"
 :license "GPL-v3"
 :author "Grigory Shepelev"

 :homepage "https://github.com/shegeley/hatis"
 :source-control (:git "https://github.com/shegeley/hatis")

 :pathname #P"src/"
 :defsystem-depends-on (#:trivial-features
                        #:alexandria
                        #:wayflan)
 :components ((:file "hatis/core" :if-feature :linux)))
