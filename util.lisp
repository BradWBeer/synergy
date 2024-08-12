(in-package :synergy)

(defun chomp (str) 
  (string-trim '(#\Space #\Newline #\Backspace #\Tab
		 #\Linefeed #\Page #\Return #\Rubout)
	       str))

(defmacro set-assoc (location key value)
  `(let ((pair (assoc ,key ,location :test #'equal)))
     (if pair
	 (setf (cdr pair) ,value)
	 (setf ,location (append ,location (list (cons ,key ,value)))))))

(defun rm-assoc (alist key &optional (cmp #'eq))
  (let ((pos (loop 
	       for x from 0
	       for (name . v) in alist
	       if (funcall cmp name key)
		 return x)))
    (when pos
      (cond ((zerop pos) (setf (car alist) (cadr alist)
			       (cdr alist) (cddr alist)))
	    (t (let ((cell (nthcdr (1- pos) alist)))
		 (setf (cdr cell) (nthcdr (1+ pos) alist))))))
      
    alist))



(defmacro mvlet* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (let* ((binding (first bindings))
	     (variables (butlast binding))
	     (expr (car (last binding))))
	`(multiple-value-bind ,variables ,expr
	   (mvlet* ,(rest bindings) ,@body)))))

;; (defmacro with-alist-slots (slot-key-pairs alist &body body)
;;   `(symbol-macrolet
;;        ,(mapcar (lambda (slot-key-pair)
;; 		  `(,(car slot-key-pair) (cdr (assoc ,(cadr slot-key-pair) ,alist :test #'equal))))
;; 	 slot-key-pairs)
;;      ,@body))

(defmacro with-alist-slots (slot-key-pairs alist &body body)
  (let ((temp-alist (gensym "TEMP-ALIST-")))
    `(let ((,temp-alist ,alist))
       (symbol-macrolet
	   ,(mapcar (lambda (slot-key-pair)
		      `(,(car slot-key-pair) (cdr (assoc ,(cadr slot-key-pair) ,temp-alist :test #'equal))))
	     slot-key-pairs)
	 ,@body))))

(defun cat (&rest args)
  (format nil "~{~A~}" args))


(defun splice (lst &rest args)
  (labels ((splice-on-symbol (list symbol splice-list)
	     (loop for item in list
		   append (if (eq item symbol)
			      splice-list
			      (list item)))))
    (if args
	(destructuring-bind (symbol value . rest) args
	  (apply #'splice (splice-on-symbol lst symbol value) rest))
	lst)))
