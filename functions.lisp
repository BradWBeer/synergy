(in-package :synergy)

(defvar *function-schemas* (make-hash-table :test 'equal))

(defun get-schema (function-name)
  "Gets the json schema for the function's symbol, might not be a user function"
  (gethash function-name *function-schemas*))

(defmacro ai-defun (name args description &body body)
  "### `ai-defun`

#### What It Does

The `ai-defun` macro defines a function and simultaneously stores a corresponding JSON schema in a hash table. This schema details the function's name, description, and parameters, including their types and any constraints.

This is for sending the function to openai's function interface. 

#### How to Use It

Use the `ai-defun` macro in place of `defun` when you want to create a function and generate a JSON schema for it. The syntax is as follows:

```lisp
(ai-defun name args description &body body)
```

- `name`: The name of the function.
- `args`: A list of the function's arguments. Each argument can be a simple name or a detailed description including the name, type (e.g., \"integer\", \"string\"), a textual description, and a required flag.
- `description`: A textual description of what the function does.
- `&body body`: The actual code of the function.

#### Example

```lisp
(ai-defun order-burger
        ((num-burgers \"integer\" \"number of burgers to order.\" t)
         (fries (\"small\" \"medium\" \"large\" \"none\") \"Would you like fries with that?\" nil))
        \"Order a burger with optional fries.\"
        (format t \"Ordering ~a burgers and ~a fries.~%\" num-burgers fries))
```

This example defines a function `order-burger`, and the corresponding JSON schema will be stored in the hash table. The function takes two parameters: `num-burgers`, an integer representing the number of burgers to order, and `fries`, a string representing the size of fries. The `num-burgers` parameter is marked as required.

The `ai-defun` macro simplifies the process of documenting functions' APIs, enabling easier integration with other systems that can interpret the JSON schema format."

  (let ((gname name)
	(gargs args)
	(gdesc description))
    `(progn
       (setf (gethash ',gname *function-schemas*)
            (create-json-schema (symbol-name ',gname) ',gdesc ',gargs))
	      
       (setf (symbol-function ',gname) 
	     (lambda (&key 
			,@(loop for (name . rest) in gargs
				collect name))
	       ,gdesc 
	       ,@(loop for i in gargs
		       when (fourth i)
			 collect `(unless ,(first i) (error (format nil "Missing required value ~S!" ',(first i)))))
	       ,@body)))))

(defun argument-to-json-property (arg)
  (let* ((name (first arg))
	 (type (second arg))
	 (description (third arg)))
    (if (listp type)
	`((,(string-downcase (symbol-name name))
	   (:TYPE . "string")
	   (:ENUM . ,type)
	   (:DESCRIPTION . ,description)))
	`((,(string-downcase (symbol-name name))
	   (:TYPE . ,type)
	   (:DESCRIPTION . ,description))))))

(defun create-json-schema (function-name description parameters)
    "Creates a JSON Schema for a given function.
**Parameters:**
- `function-name` (string): The name of the function.
- `description` (string): A textual description of the function's purpose.
- `parameters` (list): A list of the function's parameters. Each parameter is represented
  by a list containing the following elements:
    - Name (string): The parameter's name.
    - Type (string): The parameter's type (e.g. \"string\", \"integer\").
    - Description (string): A textual description of the parameter.
    - Required (boolean): `T` if the parameter is required, `NIL` otherwise.

**Returns:**
- A string containing the JSON Schema for the function.

**Example Usage:**
```lisp
(create-json-schema \"get_current_weather\"
                    \"Get the current weather in a given location\"
                    '((\"location\" \"string\" \"The city and state, e.g. San Francisco, CA\" t)
                      (\"unit\" \"string\" \"enum\" '(\"celsius\" \"fahrenheit\"))))
"
  (let* ((params)
	 (required))
    (loop for i in parameters 
	  do (let ((prop (argument-to-json-property i)))
	       (setf params (append prop params))
	       (when (fourth i)
		 (setf required (cons (caar prop) required))
		 )))
    `((:NAME . ,function-name)
      (:DESCRIPTION . ,description)
      (:PARAMETERS (:TYPE . "object")
		   (:PROPERTIES . ,(or (reverse params) (make-hash-table)))
		   ,@(when required
		       `((:REQUIRED . ,required)))))))

    
