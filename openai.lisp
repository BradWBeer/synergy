(in-package :synergy)

(defvar *chatgpt-api-key* "change this"
  "Your OpenAI API key for ChatGPT.")

(defvar *chatgpt-api-url*
  "https://api.openai.com/v1/chat/completions"
  "The API endpoint URL for ChatGPT.")

(defvar *chatgpt-completion-url* "https://api.openai.com/v1/completions")


(defvar *chatgpt-api-models-url* "https://api.openai.com/v1/models")
(defvar *chatgpt-api-embeddings-url* "https://api.openai.com/v1/embeddings")
(defvar *chatgpt-api-encoder-url* "https://openaipublic.blob.core.windows.net/gpt-2/encodings/main/encoder.json")

(defvar *openal-chat-defaults* '((:TEMPERATURE . 0.7) (:MODEL . "gpt-4-turbo")))

(defun decode-json-or-error (str)
  (let ((result (cl-json:decode-json-from-string str)))
    (if (assoc :error result)
	(error "OpenAI Error: ~A" (cdr (assoc :message (cdr (assoc :error (cl-json:decode-json-from-string result))))))
	result)))

;; This is just for completeness, it doesn't give much useful information. 
(defun list-models ()
  (decode-json-or-error
   (babel:octets-to-string
    (drakma:http-request
     *chatgpt-api-models-url*
     :additional-headers `(("Content-Type" . "application/json")
			   ("Authorization" . ,(concatenate 'string "Bearer " *chatgpt-api-key*)))
     :method :GET)
    :encoding :utf-8)))


(defun get-embedding (str)
  (cdr (assoc :embedding 
	      (cadr (assoc :data
			   (decode-json-or-error
			    (babel:octets-to-string
			     (drakma:http-request
			      *chatgpt-api-embeddings-url*
			      :content (json:encode-json-to-string `((:model . "text-embedding-ada-002")
								     (:input . ,str))) 
			      :additional-headers `(("Authorization" . ,(concatenate 'string "Bearer " *chatgpt-api-key*)))
			      :content-type "application/json"
			      :method :POST)
			     :encoding :utf-8)))))))

(defun dot-product (a b)
  (reduce #'+ (map 'list #'* a b)))

(defun normalize (a)
  (let ((n (/ (reduce (lambda (a b)
			(sqrt (+ (* a a) (* b b))))
		      a))))
    (map 'list (lambda (x)
		 (* x n)) 
	 a)))

(defun Cosine-similarity (a b)
  (dot-product (normalize a) (normalize b)))


(defun remove-u0120-prefix (str)
  (if (and (>= (length str) 5)
	   (string= (subseq str 0 5) "u0120"))
      (subseq str 5)
      str))

(defun get-encoding-tuple (s)
  (handler-case
      (let ((key (remove-u0120-prefix (read s)))
	    (_ (read-char s))
	    (value (read s))
	    (__ (read-char s)))
	(cons key value))
    (end-of-file (err)
          (return-from get-encoding-tuple nil))))

(defun get-encoder ()
  (let ((enc (babel:octets-to-string
	      (drakma:http-request
	       *chatgpt-api-encoder-url*
	       :method :GET))))
    (with-input-from-string (s (subseq enc 1 (1- (length enc))))
      (alexandria:alist-hash-table 
       (loop for tuple = (get-encoding-tuple s)
	     while tuple collect tuple) :test 'equal))))


(defun encode-next-word (word encoder)
  (multiple-value-bind (num found) (gethash word encoder)
    (if found 
	(values word num) 
	(encode-next-word (subseq word 0 (1- (length word))) encoder))))

(defun encode-word (word encoder)
  (multiple-value-bind (w id) (encode-next-word word encoder)
    
    (if (string-equal word w) 
	(list id)
	(cons id (encode-word (subseq word (length w)) encoder)))))

;; Use this one...
(defun encode-string (str encoder)
  (loop for i in (ppcre:split "\\s" str) 
	append (encode-word i encoder)))

(defun msg->chat (role content &optional name)
  (if name
      `((:ROLE . ,role)
	(:CONTENT
	 . ,content)
	(:NAME . ,name))
      `((:ROLE . ,role)
	(:CONTENT
	 . ,content))))

(defun msg (role content &optional name)
  (if name 
      (list role content name)
      (list role content)))

(defun system-msg (content &optional name)
  (msg "system" content name)) 

(defun user-msg (content &optional name)
  (msg "user" content name)) 

(defun assistant-msg (content &optional name)
  (msg "assistant" content name))

(defun function-msg (content &optional name)
  (msg "function" content name)) 

(defun chat-response->msg (response) 
  (let ((alist (cdr (assoc :message (car (cdr (assoc :choices response)))))))
    (list (cdr (assoc :role alist))
	  (cdr (assoc :content alist)))))

(defun chat-response->msgs (response)
  (loop for alist in (cdr (assoc :choices response))
	collect (with-alist-slots ((role :ROLE) (content :CONTENT)) (cdr (assoc :message alist))
		  (list role content))))

(defun openai-chat (messages &optional (model *openal-chat-defaults*) functions)
  (when messages
    (unless (is-chat (cdr (assoc :model model))) (error "This is CHAT API! You passed a non-chat moddel!"))
    (unless (listp messages) (error "This is CHAT API! You passed a string to the messages!"))
    (setf model (rm-assoc model :max--tokens))
    
    (let* ((ret (decode-json-or-error
		 (babel:octets-to-string
		  (drakma:http-request
		   *chatgpt-api-url*
		   :content (cl-json:encode-json-to-string
			     `((:MESSAGES
				,@(loop for (r m n) in messages collect (msg->chat r m n)))
			       ,@(when functions `((:FUNCTIONS . ,(map 'list #'synergy::get-schema functions))))
			       ,@model))
		   :additional-headers `(("Authorization" . ,(concatenate 'string "Bearer " *chatgpt-api-key*)))
		   :content-type "application/json"
		   :method :POST)
		  :encoding :utf-8))))
      
    (values (chat-response->msgs ret) 
	    (cdr (assoc :USAGE ret))
	    ret))))

(defun openai-chat-plus (messages &optional (model *openal-chat-defaults*) functions)
  (multiple-value-bind (msg usage ret) (openai-chat messages model functions)
    (let ((response (cadr (assoc :choices ret))))
      
      (if (string-equal (cdr (assoc :finish--reason response)) "function_call")
	  (let ((funcall (cdr 
			    (assoc :function--call 
				   (cdr (assoc :message response))))))
	  (with-alist-slots ((name :name)
			     (raw-args :arguments)) funcall
	    (let ((args (json:decode-json-from-string raw-args)))
	      (apply (symbol-function (find-symbol (string-upcase name))) 
		     (alexandria:flatten args))))
	    funcall)
    
	  (values msg usage ret)))))


(defun completion-response->str (ret)
  (chomp 
   (cdr 
    (assoc :text 
	   (cadr
	    (assoc :choices ret))))))

(defun openai-complete (prompt model &key (temp 0.0) (max-tokens 1024))
  "Send a PROMPT to ChatGPT and return the response."
  (when (is-chat (cdr (assoc :model model))) (error "This is not a chat API!"))
  (when (listp prompt) (error "This is CHAT API! You passed a list as a string!"))

  (setf model (rm-assoc model :max--tokens))
  
  (let* ((ret (decode-json-or-error
	       (babel:octets-to-string
		(drakma:http-request
		 *chatgpt-completion-url*
		  :content (cl-json:encode-json-to-string
                            `(("prompt" . ,prompt)
                              ("model" . ,model)
			      ("temperature" . ,temp)
			      ("max_tokens" . ,max-tokens)
			      ))
		  :additional-headers `(("Authorization" . ,(concatenate 'string "Bearer " *chatgpt-api-key*)))
		  :content-type "application/json"
		  :method :POST)
		:encoding :utf-8))))
    (values (completion-response->str ret) ret)))
