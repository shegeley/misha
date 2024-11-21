(defpackage :xyz.hatis.protocols.data-control
 (:use :wayflan-client :cl))
(in-package :xyz.hatis.protocols.data-control)

(xyz.shunter.wayflan.client.scanner:wl-include "wlr-data-control-unstable-v1.xml" :export t)
