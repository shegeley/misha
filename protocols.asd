(defsystem :protocols
  :description "Wayflan-Spiced Wayland Protocols Needed For Hatis"
  :version "0.0.1"
  :license "GPL-v3"
  :author "Grigory Shepelev"

  :homepage "https://github.com/shegeley/hatis"
  :source-control (:git "https://github.com/shegeley/hatis")

  :pathname #P"protocols/"

  :depends-on (:wayflan)

 :components ((:file "data-control")
              (:file "input-method"
               ;; uses enums from text-input protocol
               :depends-on ("text-input"))
              (:file "foreign-toplevel-management")
              (:file "text-input")))
