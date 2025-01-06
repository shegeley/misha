(defpackage :xyz.hatis.utils
 (:use :cl)
 (:export
  :_->- :wrap :force-output!
  :format! :interface-string->symbol))

(in-package :xyz.hatis.utils)

(defun _->- (s) (substitute #\- #\_ s))

(defun wrap (wrapper proc &rest args)
 (apply wrapper args) (apply proc args) (apply wrapper args))

(defun force-output! (&rest args) (finish-output nil))

(defun format! (&rest args)
 (apply #'wrap #'force-output! #'format args))

(defun interface-string->symbol (interface)
 (read-from-string (_->- interface)))
