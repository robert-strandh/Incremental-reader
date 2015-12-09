(cl:in-package #:incremental-reader)

(defvar *return-value*)

(defun make-token (vector)
  vector)

(defgeneric clone (state))

(defgeneric process (state syntax-type))

(defgeneric macro-character-state (syntax-type))

(defclass state () ())

(defclass macro-return (state) ())

(defmethod process ((state macro-return) syntax-type)
  (values (if (null *return-value*)
	      (list state (make-instance 'initial-state))
	      (progn (setf *return-value* (car *return-value*))
		     '()))
	  t))

(defclass initial (state)
  ())

(defmethod process ((state initial) (syntax-type whitespace))
  state)

(defmethod process ((state initial) (syntax-type single-escape))
  (change-class state 'single-escape-seen)
  state)

(defmethod process ((state initial) (syntax-type multiple-escape))
  (change-class state 'odd-multiple-escape-seen)
  state)

(defmethod process ((state initial) (syntax-type macro-character))
  (list (macro-character-state syntax-type)
	(make-instance 'macro-return)))

(defclass token-accumulation (state)
  ((%token :initform (make-array 0 :adjustable t :fill-pointer 0)
	   :initarg :token
	   :reader token)))

(defun clone-vector (vector)
  (make-array (length vector)
	      :adjustable t
	      :fill-pointer (length vector)
	      :initial-contents vector))

(defmethod clone ((state token-accumulation))
  (make-instance (class-of state)
    :token (clone-vector (token state))))

(defclass single-escape-seen (token-accumulation)
  ())

(defmethod process ((state single-escape-seen) syntax-type)
  (vector-push-extend (character syntax-type) (token state))
  (change-class state 'even-multiple-escape-seen)
  state)

(defclass even-multiple-escape-seen (token-accumulation)
  ())

(defclass even-multiple-and-single-escape-seen (token-accumulation)
  ())

(defmethod process ((state even-multiple-escape-seen)
		    (syntax-type constituent))
  (vector-push-extend (character syntax-type) (token state))
  state)

(defmethod process ((state even-multiple-escape-seen)
		    (syntax-type non-terminating-macro))
  (vector-push-extend (character syntax-type) (token state))
  state)

(defmethod process ((state even-multiple-escape-seen)
		    (syntax-type single-escape))
  (change-class state 'even-multiple-and-single-escape-seen)
  state)

(defmethod process ((state even-multiple-escape-seen)
		    (syntax-type multiple-escape))
  (change-class state 'odd-multiple-escape-seen)
  state)

(defmethod process ((state even-multiple-escape-seen)
		    (syntax-type whitespace))
  (setf *return-value* (make-token (token state)))
  (values '() t))

(defmethod process ((state even-multiple-and-single-escape-seen)
		    syntax)
  (vector-push-extend (character syntax) (token state))
  (change-class state 'even-multiple-escape-seen)
  state)

(defclass odd-multiple-escape-seen (token-accumulation)
  ())

(defclass odd-multiple-and-single-escape-seen (token-accumulation)
  ())

(defmethod process ((state odd-multiple-escape-seen)
		    (syntax-type constituent))
  (vector-push-extend (character syntax-type) (token state))
  state)

(defmethod process ((state odd-multiple-escape-seen)
		    (syntax-type macro-character))
  (vector-push-extend (character syntax-type) (token state))
  state)

(defmethod process ((state odd-multiple-escape-seen)
		    (syntax-type whitespace))
  (vector-push-extend (character syntax-type) (token state))
  state)

(defmethod process ((state odd-multiple-escape-seen)
		    (syntax-type single-escape))
  (change-class state 'odd-multiple-and-single-escape-seen)
  state)

(defmethod process ((state odd-multiple-escape-seen)
		    (syntax-type multiple-escape))
  (change-class state 'even-multiple-escape-seen)
  state)

(defmethod process ((state odd-multiple-and-single-escape-seen)
		    syntax)
  (vector-push-extend (character syntax) (token state))
  (change-class state 'odd-multiple-escape-seen)
  state)