(cl:in-package #:incremental-reader)

(defclass open-parenthesis (terminating-macro)
  ())

(defclass closing-parenthesis (terminating-macro)
  ())

(defclass list-reader (state)
  ((%elements :initform '() :initarg :elements :reader elements)))

(defmethod clone ((state list-reader))
  (make-instance (class-of state)
    :elements (elements state)))

(defmethod macro-character-state ((syntax-type open-parenthesis))
  (make-instance 'list-reader))

(defclass end-list-reader (state)
  ())

(defmethod macro-character-state ((syntax-type closing-parenthesis))
  (make-instance 'end-list-reader))

(defmethod process ((state list-reader) syntax-type)
  (values (list state (make-instance 'initial-state))
	  t))

(defmethod process ((state end-list-reader) syntax-type)
  (values (if (typep (first *stack*) 'list-reader)
	      (progn (setf *return-value*
			   (list (reverse (elements (first *stack*)))))
		     (pop *stack*)
		     ;; We do not push any new state on the stack.
		     '())
	      (make-instance 'error))
	  ;; We do not consume the current character.
	  t))
