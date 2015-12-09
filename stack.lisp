(cl:in-package #:incremental-reader)

;;; The stack is represented as a list of instances of (subclasses of)
;;; the class STATE.
(defparameter *stack* '())

(defun canonicalize-state-list-designator (state-list-designator)
  (if (listp state-list-designator)
      state-list-designator
      (list state-list-designator)))

(defun update-stack (state-list-designator)
  (setf *stack*
	(append (canonicalize-state-list-designator state-list-designator)
		*stack*)))
