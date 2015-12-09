(cl:in-package #:incremental-reader)

(defclass syntax-type ()
  ((%character :initarg :character :reader character)))

(defclass whitespace (syntax-type)
  ())

(defclass constituent (syntax-type)
  ())

(defclass single-escape (syntax-type)
  ())

(defclass multiple-escape (syntax-type)
  ())

(defclass macro-character (syntax-type)
  ())

(defclass terminating-macro (macro-character)
  ())

(defclass single-quote (terminating-macro)
  ())

(defclass double-quote (terminating-macro)
  ())

(defclass semicolon (terminating-macro)
  ())

(defclass non-terminating-macro (macro-character)
  ())


