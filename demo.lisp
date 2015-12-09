(cl:in-package #:incremental-reader)

(defun make-syntax (character)
  (cond ((member character '(#\Space #\Tab #\Newline))
	 (make-instance 'whitespace
	   :character character))
	((eql character #\()
	 (make-instance 'open-parenthesis
	   :character character))
	((eql character #\))
	 (make-instance 'closing-parenthesis
	   :character character))
	(t
	 (make-instance 'constituent
	   :character character))))

(defun maybe-push (designator)
  (cond ((null designator)
	 nil)
	((listp designator)
	 (setf *stack* (append designator *stack*)))
	(t
	 (push designator *stack*))))

(defun initialize-stack ()
  (setf *stack*
	(list (make-instance 'initial))))

(defun demo (string)
  (initialize-stack)
  (loop for character across string
	for syntax = (make-syntax character)
	for result = (multiple-value-list (process (pop *stack*) syntax))
	do (when (null *stack*)
	     (return-from demo *return-value*))
	   (maybe-push (car result))
	   (loop until (null (cadr result))
		 do (when (null *stack*)
		      (return-from demo *return-value*))
		    (setf result
			  (multiple-value-list (process (pop *stack*) syntax)))
		    (maybe-push (car result)))))

			  
