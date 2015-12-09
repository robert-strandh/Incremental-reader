(cl:in-package #:incremental-reader)

;;; The stack is represented as a list of instances of (subclasses of)
;;; the class STATE.
(defparameter *stack* '())

(defun update-stack (state-list-designator)
  (cond ((null state-list-designator)
	 nil)
	((listp state-list-designator)
	 (setf *stack* (append state-list-designator *stack*)))
	(t
	 (push state-list-designator *stack*)))
  *stack*)
