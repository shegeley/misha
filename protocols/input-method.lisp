(defpackage :xyz.hatis.protocols.input-method
 (:use :wayflan-client :xyz.hatis.protocols.text-input))
(in-package :xyz.hatis.protocols.input-method)

(xyz.shunter.wayflan.client.scanner:wl-include "input-method-unstable-v2.xml" :export t)
