(cl:in-package #:incremental-reader)

;;; This variable is used to communicate between callee and caller.
;;; When the reader returns, this variable contains a normal return
;;; value.  When a reader macro processor returns, this variable
;;; contains a list of values, so that the empty list means that no
;;; value was returned and the reader must be restarted.
(defvar *return-value*)

;;; This generic function is called in order to clone a state.  States
;;; do not have to be cloned for every character read.  They need to
;;; be cloned only when the previous state of the reader must be
;;; preserved.
(defgeneric clone (state))

;;; This generic function is called repeatedly with the current state
;;; of the reader and for each character.
;;;
;;; Before this generic function is called, the current state has been
;;; popped off the stack.
;;;
;;; Method may assume that STATE can be mutated, including by changing
;;; its class.
;;;
;;; One or two values should be returned by each method.  The first
;;; value is used to represent how that stack should change.  In the
;;; most general case, the first value is a list of states to be
;;; APPENDed to the beginning (top) of the stack.  The convention
;;; includes returning the empty list, effectively popping the stack.
;;; As a shorthand, the first value can be a single state, which
;;; designates a singleton list.  The second value is a Boolean.  When
;;; absent or NIL, this indicates to the caller that the character
;;; represented by SYNTAX-TYPE has been fully processed.  When the
;;; second value is true, the same syntax type should be processed
;;; again.
(defgeneric process (state syntax-type))

(defgeneric macro-character-state (syntax-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STATE.
;;;
;;; This is the base class for all state classes.

(defclass state () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; State class MACRO-RETURN.
;;;
;;; This state means that a macro character has been fully processed.
;;; There are two possibilities for *RETURN-VALUE*.  Either it is the
;;; empty list, meaning that no value was returned form processing the
;;; macro character.  In that case, we restart the reader in the
;;; initial state.  Or else, *RETURN-VALUE* is a singleton list,
;;; meaning that the CAR of that list was the value returned from
;;; processing the macro character.  In that case, we set
;;; *RETURN-VALUE* to that return value, and we return the empty list,
;;; meaning we pop the stack.

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
