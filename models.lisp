(in-package :synergy)

(defun get-model-chat? (alist)
  (cdr (assoc :chat alist)))
  
(defun get-model-function (alist)
  (if (cdr (assoc :chat alist)) 
      #'openai-chat
      #'openai-complete))

(defun get-model-data (alist &optional temp)
  (let ((model (cdr (assoc :model alist))))
    (when temp 
      (set-assoc model :temperature temp))
    model))

(defun find-best-models (tokenSize &key (smartness nil) (temp 0) (functions nil) (chat))
  ;; Filter the models based on the given tokenSize and optionally smartness
  (map 'list (lambda (x)
	       (cons (get-model-data x temp) 
		     (get-model-function x)))
       (sort 
	(loop for i in *models* 
	      if (and (<= tokensize (cdr (assoc :max--tokens i)))
		      (or (null functions)
			  (cdr (assoc :functions i)))
		      (or (null chat)
			  (cdr (assoc :chat i)))
		      (or (null smartness) 
			  (<= smartness (cdr (assoc :smartness i)))))
		collect i)
	
	(lambda (a b)
	  (let ((cost-a (cdr (assoc :costInput a)))
		(cost-b (cdr (assoc :costInput b))))
	    (if (= cost-a cost-b)
		(> (length (cdr (assoc :modelName a)))
		   (length (cdr (assoc :modelName b))))
		(< (cdr (assoc :costInput a))
		   (cdr (assoc :costInput b)))))))))

(defun find-best-model (tokenSize &key (smartness nil) (temp 0) (functions nil) (chat))
  (let ((model (car (find-best-models tokenSize :smartness smartness :temp temp :functions functions :chat chat))))
    (values (car model)
	    (cdr model))))
	      

(defun create-openai-chat-model (model &key 
					 (temperature 0) 
					 (top_p 1) 
					 (n 1) 
					 stop
					 max_tokens
					 (presence_penalty 0) 
					 (frequency_penalty 0) 
					 logit_bias)
  (let* ((params `(("model" . ,model)
		   ("temperature" . ,temperature)
		   ("top_p" . ,top_p)
		   ("n" . ,n)
		   ("stop" . ,stop)
		   ("max_tokens" . ,max_tokens)
		   ("presence_penalty" . ,presence_penalty)
		   ("frequency_penalty" . ,frequency_penalty)
		   ("logit_bias" . ,logit_bias)))
	 (non-null-params (remove-if (lambda (param) (null (cdr param))) params)))
    non-null-params))

(defun is-chat (model)
  (loop for i in synergy::*models*
	do (when (string-equal model (cdr (assoc :modelname i)))
	     (return-from is-chat (values t #'openai-chat))))
  (values nil #'openai-complete))

(defun get-model (name)
  (loop for i in *models*
	when (string-equal (cdr (assoc :modelName i)) name)
	  do (return (values (cdr (assoc :model i))
			     (if (cdr (assoc :chat i)) 
				 #'openai-chat
				 #'openai-complete)))))


(defparameter *models* '(((:modelName . "gpt-4") (:chat . t) (:max--tokens . 8192) (:smartness . 7) (:costInput . 0.03) (:costOutput . 0.06) (:model . ((:temperature . 0) (:model . "gpt-4") (:max--tokens . 8192))))
			 ((:modelName . "gpt-4-0613") (:chat . t) (:functions . t) (:max--tokens . 8192) (:smartness . 7) (:costInput . 0.03) (:costOutput . 0.06) (:model . ((:temperature . 0) (:model . "gpt-4-0613") (:max--tokens . 8192))))
			 ((:modelName . "gpt-3.5-turbo") (:chat . t) (:max--tokens . 4096) (:smartness . 6) (:costInput . 0.0015) (:costOutput . 0.002) (:model . ((:temperature . 0) (:model . "gpt-3.5-turbo") (:max--tokens . 4096))))
			 ((:modelName . "gpt-3.5-turbo-16k") (:chat . t) (:max--tokens . 16384) (:smartness . 6) (:costInput . 0.003) (:costOutput . 0.004) (:model . ((:temperature . 0) (:model . "gpt-3.5-turbo-16k") (:max--tokens . 16384))))
			 ((:modelName . "gpt-3.5-turbo-0613") (:chat . t) (:functions . t) (:max--tokens . 4096) (:smartness . 6) (:costInput . 0.0015) (:costOutput . 0.002) (:model . ((:temperature . 0) (:model . "gpt-3.5-turbo-0613") (:max--tokens . 4096))))
			 ((:modelName . "gpt-3.5-turbo-16k-0613") (:chat . t) (:functions . t) (:max--tokens . 16384) (:smartness . 6) (:costInput . 0.003) (:costOutput . 0.004) (:model . ((:temperature . 0) (:model . "gpt-3.5-turbo-16k-0613") (:max--tokens . 16384))))
			 ((:modelName . "ada") (:max--tokens . 2049) (:smartness . 3) (:costInput . 0.0004) (:costOutput . 0.0016) (:model . ((:temperature . 0) (:model . "ada") (:max--tokens . 2049))))
			 ((:modelName . "babbage") (:max--tokens . 2049) (:smartness . 4) (:costInput . 0.0006) (:costOutput . 0.0024) (:model . ((:temperature . 0) (:model . "babbage") (:max--tokens . 2049))))
			 ((:modelName . "curie") (:max--tokens . 2049) (:smartness . 5) (:costInput . 0.003) (:costOutput . 0.012) (:model . ((:temperature . 0) (:model . "curie") (:max--tokens . 2049))))
			 ((:modelName . "davinci") (:max--tokens . 2049) (:smartness . 6) (:costInput . 0.03) (:costOutput . 0.12) (:model . ((:temperature . 0) (:model . "davinci") (:max--tokens . 2049))))
			 ((:modelName . "text-curie-001") (:max--tokens . 2049) (:smartness . 5) (:costInput . 0.003) (:costOutput . 0.012) (:model . ((:temperature . 0) (:model . "text-curie-001") (:max--tokens . 2049))))
			 ((:modelName . "text-babbage-001") (:max--tokens . 2049) (:smartness . 4) (:costInput . 0.0006) (:costOutput . 0.0024) (:model . ((:temperature . 0) (:model . "text-babbage-001") (:max--tokens . 2049))))
			 ((:modelName . "text-ada-001") (:max--tokens . 2049) (:smartness . 3) (:costInput . 0.0004) (:costOutput . 0.0016) (:model . ((:temperature . 0) (:model . "text-ada-001") (:max--tokens . 2049))))
			 ((:modelName . "davinci") (:max--tokens . 2049) (:smartness . 6) (:costInput . 0.03) (:costOutput . 0.12) (:model . ((:temperature . 0) (:model . "davinci") (:max--tokens . 2049))))
			 ((:modelName . "curie") (:max--tokens . 2049) (:smartness . 5) (:costInput . 0.003) (:costOutput . 0.012) (:model . ((:temperature . 0) (:model . "curie") (:max--tokens . 2049))))
			 ((:modelName . "babbage") (:max--tokens . 2049) (:smartness . 4) (:costInput . 0.0006) (:costOutput . 0.0024) (:model . ((:temperature . 0) (:model . "babbage") (:max--tokens . 2049))))
			 ((:modelName . "ada") (:max--tokens . 2049) (:smartness . 3) (:costInput . 0.0004) (:costOutput . 0.0016) (:model . ((:temperature . 0) (:model . "text-ada-001") (:max--tokens . 2049))))))

