;;;; synergy.asd

(asdf:defsystem #:synergy
  :description "Describe synergy here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-json #:cl-ppcre #:drakma #:babel) ;; #:cl-html-parse #:osicat #:uuid #:bordeaux-threads #:local-time
  :components ((:file "package")
	       (:file "util")
	       (:file "functions")
	       (:file "models")
	       (:file "parsers")
	       (:file "openai")
               (:file "synergy")))
