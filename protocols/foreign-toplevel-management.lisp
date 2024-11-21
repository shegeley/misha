(defpackage :xyz.hatis.protocols.foreign-toplevel-management
  (:use :wayflan-client :cl))
(in-package :xyz.hatis.protocols.foreign-toplevel-management)

(xyz.shunter.wayflan.client.scanner:wl-include "wlr-foreign-toplevel-management-unstable-v1.xml" :export t)
