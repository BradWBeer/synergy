;;;; package.lisp

(defpackage #:synergy
  (:use #:cl)
  (:export
   #:openai-chat
   #:openai-complete
   #:system-msg
   #:user-msg
   #:assistant-msg
   #:function-msg
   #:list-models
   #:get-model
   #:find-best-model
   #:find-best-models
   #:with-alist-slots

   #:get-encoder
   #:encode-string
   
   #:get-embedding
   #:dot-product
   #:normalize
   #:Cosine-similarity

   
   #:create-json-schema
   #:ai-defun
   #:get-schema
   
   #:cat
   #:splice
   #:chomp
   #:set-assoc
   #:mvlet*
   ))
