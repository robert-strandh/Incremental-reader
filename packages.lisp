(cl:in-package #:common-lisp-user)

(defpackage #:incremental-reader
  (:use #:common-lisp)
  (:shadow #:character #:error)
  (:export))
