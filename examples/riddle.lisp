(ql:quickload :synergy)
(use-package :synergy)

(ql:quickload :cl-mysql)
(use-package :cl-mysql)

(defparameter *host* "localhost")
(defparameter *user* "user")
(defparameter *password* "pw")

(defparameter *connection* 
  (connect :host *host* :user *user* :password *password*))

(use "Riddle")

(defparameter *riddles* nil)
(defparameter *answers* nil)
(defparameter *hints* nil)
(defparameter *difficulties* nil)
(defparameter *bool* nil)

(defmacro fquery (&rest args)
  `(query (format nil ,@args)))

(defun insert-riddle (riddle answer hint1 hint2 hint3 difficulty)
  (fquery "INSERT INTO Riddles (riddle_text, answer, hint1, hint2, hint3, difficulty_level) VALUES (~S, ~S, ~S, ~S, ~S, ~A)"
	  riddle answer hint1 hint2 hint3 difficulty))

(defun insert-riddle-from-alist (alist)
  (let* ((riddle (cdr (assoc :RIDDLE alist)))
	 (answer (cdr (assoc :ANSWER alist)))
	 (hint1 (cdr (assoc :HARD alist)))
	 (hint2 (cdr (assoc :MEDIUM alist)))
	 (hint3 (cdr (assoc :EASY alist)))
	 (difficulty (cdr (assoc :DIFFICULTY alist))))
	 	 
    (insert-riddle riddle answer hint1 hint2 hint3 difficulty))
  alist)

(defun get-all-answers-in-db ()
  (alexandria:flatten 
   (caar 
    (fquery "select distinct answer from Riddles;"))))

(ai-defun handle-riddle
        ((riddle "string" "The text of the riddle." t)
         (answer "string" "A single word solution to the riddle." t))
    "Create a riddle."
    (setf *riddles* (push riddle *riddles*)
	  *answers* (push answer *answers*)))

(defun convert-db-blob-to-alist (db-blob)
  (let* ((data (caaar db-blob)) ; This assumes that your data is in the nested form you provided
	 (id (nth 0 data))
	 (riddle (nth 1 data))
	 (answer (nth 2 data))
	 (hint1 (nth 3 data))
	 (hint2 (nth 4 data))
	 (hint3 (nth 5 data))
	 (difficulty (nth 6 data)))
    `((:ID ,id)
      (:RIDDLE ,riddle)
      (:ANSWER ,answer)
      (:HARD ,hint1)
      (:MEDIUM ,hint2)
      (:EASY ,hint3)
      (:DIFFICULTY ,difficulty))))


(defun get-random-riddle ()
  (convert-db-blob-to-alist
   (query "SELECT * FROM Riddles ORDER BY RAND() LIMIT 1;")))
  
(defun insert-alternative-answer (riddle-id alternative-answer)
  (fquery "INSERT INTO AlternativeAnswers (RiddleID, AlternativeAnswer) VALUES (~A, ~S)"
	  riddle-id alternative-answer))

(defun fetch-alternative-answers (riddle-id)
					; Query the database to get all alternative answers for a specific riddle ID
					; Returns a list of alternative answers
  (alexandria:flatten 
   (caar 
    (fquery "SELECT AlternativeAnswer FROM AlternativeAnswers WHERE RiddleID = ~A" riddle-id))))

(defun compare-possible-answer (riddle-id possible-answer)
  (let ((existing-answers (fetch-alternative-answers riddle-id)))
    (member possible-answer existing-answers :test #'string-equal)))


(ai-defun handle-hints
    ((hard "string" "First (little helpful) hint for the riddle." t)
     (medium "string" "Second (moderately helpful) hint for the riddle." t)
     (easy "string" "Third (almost giving the user the answer) hint for the riddle." t))
    "Create a series of hints for a a riddle."
  (setf *hints* (push (list hard medium easy) *hints*)))

(ai-defun handle-difficulty
    ((difficulty "integer" "The difficulty of the riddle from 1 to 10 with 1 being obvious and 10 being impossible." t))
    "Rate the difficulity of the riddle"
  (format t "Difficulty = ~A~%" difficulty)
  (setf *difficulties* (push difficulty *hints*)))

(ai-defun answer-true-or-false 
    ((bool ("true" "false") "The true or false value to return for the question." t))
    "Return either true or false."
  (setf *bool* bool))

(defparameter ai (find-best-model 1024 :smartness 7 :chat t :temp .7))

(defun make-riddle ()
  (with-alist-slots ((riddle :riddle)
		     (answer :answer)) 
	 
      (json:decode-json-from-string 
       (cdr 
	(assoc :arguments 
	       (synergy::openai-chat-plus (list (user-msg (format nil "You are a great riddler! You will create a riddle with a one word answer that is not a member of the following list:  ~{~A~^, ~}" (or (get-all-answers-in-db) (list "No list items yet!"))))) ai '(handle-riddle)))))
    (values riddle answer)))
  
(defun make-hints (riddle answer)
  (with-alist-slots ((hard :hard)
		     (medium :medium)
		     (easy :easy))       
      (json:decode-json-from-string 
       (cdr 
	(assoc :arguments 
	       
	       (synergy::openai-chat-plus (list (user-msg (format nil "You are a great riddler! You will take the following riddle and answer and create three hints (use the handle-hints function):~%~%~A~%Answer: ~A" riddle answer))) ai '(handle-hints)))))
    (values easy medium hard)))
  
(defun measure-difficulty (riddle answer)
  (with-alist-slots ((difficulty :difficulty))
      (json:decode-json-from-string 
       (cdr 
	(assoc :arguments 

	       (synergy::openai-chat-plus (list (user-msg (format nil "You are a great riddler! You will take the following riddle and answer and rate its difficulty using the handle-difficulty function:~%~%~A~%Answer: ~A" riddle answer))) ai '(handle-difficulty)))))
    difficulty))

(defun generate-complete-riddle (&optional (max-retries 3))
  (let ((retries 0))
    (loop
          (handler-case
	      (progn
		(insert-riddle-from-alist
		 (multiple-value-bind (riddle answer) (make-riddle)
		   (multiple-value-bind (easy medium hard) (make-hints riddle answer)
		     (let ((difficulty (measure-difficulty riddle answer)))
		       `((:riddle ,riddle)
			 (:answer ,answer)
			 (:hard ,hard)
			 (:medium ,medium)
			 (:easy ,easy)
			 (:difficulty ,difficulty))))))
		(return "Riddle generated and inserted successfully."))

	    ;; In case an error occurs, catch it and retry
	    (error (e)
	      (format t "An error occurred: ~A~%" e)
	      (if (>= retries max-retries)
		  (return "Exceeded max retries. Riddle not inserted.")
		  (incf retries)
		  (format t "Retrying... (~D/~D)~%" retries max-retries)))))))



(defun is-answer-close-enough? (riddle user-answer)
  (with-alist-slots ((riddle :riddle)
		     (actual-answer :answer)
		     (id :id)) riddle
    ;; first check if the database has this answer...
    (or (compare-possible-answer id user-answer)
        (with-alist-slots ((boolean :boolean))
	    (json:decode-json-from-string 
	     (cdr 
	      (assoc :arguments 
		     (synergy::openai-chat-plus (list (user-msg (format nil "I have a riddle and an answer provided for it. I would like to know if the given answer is close enough to solving the riddle. Here is the riddle:

Riddle: ~S 

The correct answer is: ~S
The provided answer is: ~S

Is the provided answer close enough to be considered a correct solution to the riddle? Answer using the function answer-true-or-false." riddle actual-answer user-answer))) ai '(answer-true-or-false)))))
	  (when (string-equal "true" *bool*)
	    ;; I need to add a new table that will
	    (insert-alternative-answer id user-answer)
	    t)))))
  
