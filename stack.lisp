(cl:in-package #:incremental-reader)

;;; The stack is represented as a list of instances of (subclasses of)
;;; the class STATE.
(defparameter *stack* '())
