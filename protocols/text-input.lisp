(defpackage :xyz.hatis.protocols.text-input
  (:use :wayflan-client :cl))
(in-package :xyz.hatis.protocols.text-input)

(xyz.shunter.wayflan.client.scanner:wl-include "/unstable/text-input/text-input-unstable-v3.xml" :export t)
