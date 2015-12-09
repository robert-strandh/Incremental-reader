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

(defmethod process ((state list-reader) (syntax-type closing-parenthesis))
  (setf *return-value* (list (reverse (elements state))))
  '())
