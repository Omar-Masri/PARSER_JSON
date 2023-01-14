;;;; -*- Mode: Lisp -*-
;;;; jsonparse.lisp

;;;; jsonparse

(defun jsonparse (json-input)
  (if (stringp json-input)
      (let ((j (json json-input)))
	(if (null j)
	    (error "ERROR: syntax error")
	  (second j)))
    (error "ERROR: json only accepts strings")))

(defun json-ws (json-input)
  (or (cond
       ((string= "" json-input) ""))
      (let ((f (char json-input 0)))
	(if (or (char= f #\Space)
		(char= f #\Newline)
		(char= f #\Return)
		(char= f #\Tab))
	    (json-ws (subseq json-input 1))
	  json-input))))

(defun json (json-input)
  (let ((j (json-element json-input)))
    (if (string= "" (car j)) j NIL)))

(defun json-value (json-input)
  (or (dcg-handle json-input (list #'json-obj))
      (dcg-handle json-input (list #'json-array))
      (dcg-handle json-input (list #'json-string))
      (dcg-handle json-input (list #'json-number))
      (dcg-match json-input "true" :ret 'true)
      (dcg-match json-input "false" :ret 'false)
      (dcg-match json-input "null" :ret 'null)))

(defun json-element (json-input)
  (dcg-handle json-input (list #'json-ws #'json-value #'json-ws)))

;; ------- objects --------

(defun json-obj (json-input)
  (or (dcg-handle json-input
		  (list "{" #'json-ws "}")
		  (lambda (f)
		    '(JSONOBJ)))
      (dcg-handle json-input
		  (list "{" #'json-ws #'json-members #'json-ws
			"}")
		  (lambda (f)
		    (append '(JSONOBJ)
			    (first (second f)))))))

(defun json-members (json-input)
  (let ((f (dcg-handle json-input
		       (list #'json-member))))
    (if (null (car f))
	nil
      (let ((m (dcg-handle (car f)
			   (list "," #'json-members)
			   (lambda (g)
			     (second g)))))
	(if (null (car m))
	    (list (car f)
		  (cdr f))
	  (list (car m)
		(append (cdr f)
			(first (second m)))))))))

(defun json-member (json-input)
  (dcg-handle json-input
	      (list #'json-ws #'json-string #'json-ws ":"
		    #'json-ws #'json-value #'json-ws)
	      (lambda (f)
		(second f))))

;; ------- array --------

(defun json-array (json-input)
  (or (dcg-handle json-input
		  (list "[" #'json-ws "]")
		  (lambda (f)
		    '(JSONARRAY)))
      (dcg-handle json-input
		  (list "[" #'json-ws #'json-elements #'json-ws
			"]")
		  (lambda (f)
		    (append '(JSONARRAY)
			    (first (second f)))))))

(defun json-elements (json-input)
  (let ((f (dcg-handle json-input
		       (list #'json-element))))
    (if (null (car f))
	nil
      (let ((m (dcg-handle (car f)
			   (list "," #'json-elements)
			   (lambda (g)
			     (second g)))))
	(if (null (car m))
	    (list (car f)
		  (cdr f))
	  (list (car m)
		(append (cdr f)
			(first (second m)))))))))

(defun json-element (json-input)
  (dcg-handle json-input
	      (list #'json-ws #'json-value #'json-ws)
	      (lambda (f)
		(first (second f)))))

;;;; numbers

(defun json-number (json-input)
  (dcg-handle json-input
	      (list #'json-integer #'json-fraction #'json-exponent)
	      (lambda (f)
		(eval (read-from-string (lis-str (second f)))))))

(defun json-fraction (json-input)
  (let ((m (dcg-match json-input ".")))
    (if (null m)
	(list json-input "")
      (dcg-handle (first m)
		  (list #'json-digits)
		  (lambda (f)
		    (concatenate 'string
				 "."
				 (first (second f))))))))

(defun json-exponent (json-input)
  (let ((m (dcg-match json-input "e" :ignore-case T)))
    (if (null m)
	(list json-input "")
      (dcg-handle (first m)
		  (list #'json-sign #'json-digits)
		  (lambda (f)
		    (concatenate 'string
				 "e"
				 (lis-str (second f))))))))
(defun json-sign (e)
  (or (dcg-match e "+")
      (dcg-match e "-")
      (dcg-match e "")))

(defun json-integer (json-input)
  (let ((m (dcg-match json-input "-")))
    (if (null m)
	(json-integer-h json-input)
      (dcg-handle (first m)
		  (list #'json-integer-h)
		  (lambda (f)
		    (concatenate 'string
				 "-"
				 (first (second f))))))))

(defun json-integer-h (json-input)
  (or (dcg-handle json-input
		  (list #'json-onenine #'json-digits)
		  (lambda (f)
		    (lis-str (second f))))
      (dcg-handle json-input
		  (list #'json-digit)
		  (lambda (f)
		    (lis-str (second f))))))

(defun json-digit (json-input)
  (or (dcg-match json-input "0")
      (json-onenine json-input)))

(defun json-onenine (json-input)
  (if (string= "" json-input)
      NIL
    (let ((f (char json-input 0)))
      (cond
       ((< 48 (char-code f)
	   58)
	(list (subseq json-input 1)
	      (string f)))))))

(defun json-digits (json-input)
  (or (dcg-handle json-input
		  (list #'json-digit #'json-digits)
		  (lambda (f)
		    (lis-str (second f))))
      (dcg-handle json-input
		  (list #'json-digit)
		  (lambda (f)
		    (lis-str (second f))))))

;;;; strings

(defun json-string (json-input)
  (dcg-handle json-input
	      (list "\"" #'json-charachters "\"")))

(defun json-charachters (json-input)
  (let ((f (json-charachters-h json-input)))
    (if (= f -1)
	NIL
      (list (subseq json-input f)
	    (subseq json-input 0 f)))))

(defun json-charachters-h (json-input &optional (n 0))
  (let ((m (eq (json-char json-input n) #\\)))
    (if (null m)
	(let ((f (json-char json-input n)))
	  (if
	   f
	   (json-charachters-h json-input (+ n 1))
	   n))
	(let ((f (json-escape (json-char json-input (+ n 1) T))))
	  (if
	   f
	   (json-charachters-h json-input (+ n 2))
	   -1))
	)))

(defun json-char (json-input n &optional esc)
  (if (<= (length json-input) n)
      NIL
    (let ((f (char json-input n)))
      (cond
       ((or esc
	    (char/= f #\")) f)))))

(defun json-escape (e)
  (or (eq e #\") 
      (eq e #\\)
      (eq e #\/)
      (eq e #\b)
      (eq e #\f)
      (eq e #\n)
      (eq e #\r)
      (eq e #\t)
      (eq e #\u)))

;;;; dcg stuff

;; tail-recursive
(defun dcg-and (json-input &optional l acc)
  (if (null json-input)
      NIL
    (if (null l)
	(list json-input acc)
      (let ((f (first l)))
	(typecase f
	  (function (let ((c (funcall f json-input)))
		      (if (consp c)
			  (dcg-and (car c)
				   (rest l)
				   (append-e acc
					     (second c)))
			(dcg-and c
				 (rest l)
				 acc))))
	  (string (dcg-and (car (dcg-match json-input f))
			   (rest l)
			   acc)))))))

(defun dcg-match (json-input y &key ret ignore-case)
  (let ((l (length y)))
    (cond
     ((>= (length json-input) l)
      (let ((x (subseq json-input 0 l)))
	(cond
	 ((funcall (if ignore-case #'string-equal #'equal)
		   x
		   y)
	  (list (subseq json-input l)
		(or ret y)))))))))

(defun dcg-handle (json-input l
			      &optional
			      (fun (lambda (f)
				     (first (second f)))))
  (let ((f (dcg-and json-input l)))
    (cond
     ((car f)
      (list (car f)
	    (funcall fun f))))))


;;;; jsonaccess

;;; jsonaccess(json-obj &rest fields)
;;; function to access to a determinate value using the path specified by fields
;;; the function generates an error if: 
;;; 1) the json-obj is an empty json
;;; 2) the path doesn't exist
;;; 3) the path goes out of bound (for array)

(defun jsonaccess (json-obj &rest fields)
  (cond
    ((eql (length json-obj)
	  1)
     (error "no match: empty json"))
    ((and (eql 'JSONOBJ
	       (first json-obj))
	  (stringp (first fields)))
     (if (equal (first (second json-obj)) (first fields))
	 (if (eql (length fields)
		  1)
	     (second (second json-obj))
	     (apply #'jsonaccess
		    (second (second json-obj))
		    (rest fields)))
	 (if (null (third json-obj))
	     (error "no match: key not found")
	     (apply #'jsonaccess
		    (remove (second json-obj)
			    json-obj
			    :count 1)
		    fields))))
    ((and (eql 'JSONARRAY
	       (first json-obj))
	  (integerp (first fields))
	  (>= (first fields) 0))
     (if (< (first fields) (- (length json-obj)
			      1))
	 (if (= (length fields) 1)
	     (nth (+ (first fields)
		     1)
		  json-obj)
	     (apply #'jsonaccess
		    (nth (+ (first fields)
			    1)
			 json-obj)
		    (rest fields)))
	 (error "no match: index out of bound")))
    (T (error "something went wrong!"))))

;;;; input e output

;;; jsonread(filename)
;;; read a json from the file "filename" and call the jsonparse function that
;;; return the json in a parsed form that is easier to manipulate in Lisp

(defun jsonread (filename)
  (with-open-file (in filename :direction :input
			       :if-does-not-exist :error)
    (readfile in "")))

(defun readfile (in res-string) 
  (let ((s (read-line in nil 'eof)))
    (if (eq s 'eof) (jsonparse res-string)
	(readfile in (concatenate 'string res-string s (string #\Newline))))))


;;; jsondump(JSON filename)
;;; write the JSON passed after calling the jsonencode function
;;; into the file specified by the path filename

(defun jsondump (json filename)
  (with-open-file (out filename :direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
    (format out (jsonencode json 0 1))))


;;; jsonencode(parsed-json type tab)
;;; reverts a JSONOBJ in a standard JSON
;;; the field type is used for different situation, in particular:
;;; 0 if it's not a list returns the value itself (parsed-json), 
;;;   otherwise checks if it's a JSONOBJ or a JSONARRAY 
;;;   and reacts accordingly 
;;; 1 expects that parsed-json is the body of a JSONOBJ, so a list of lists
;;; 2 expects that parsed-json is the body of a JSONARRAY, so a flat list

(defun jsonencode (parsed-json type tab)
    (cond    
      ((zerop type)
       (cond ((not (listp parsed-json)) 
              (if (stringp parsed-json)
		  (concatenate 'string "\"" parsed-json "\"")
		  parsed-json))
             ((eql 'JSONOBJ (first parsed-json))
	      (if (null (rest parsed-json))
		  "{}"
		  (encode-obj-arr parsed-json 1 tab)))
	     ((eql 'JSONARRAY (first parsed-json))
	      (if (null (rest parsed-json))
		  "[]"
		  (encode-obj-arr parsed-json 2 tab)))))
      ((= type 1)
       (if (null (rest parsed-json))
	   (encode-obj parsed-json tab)
	   (encode-rest parsed-json 1 tab)))
      ((= type 2)
       (if (null (rest parsed-json)) 
	   (get-element-arr parsed-json tab)
	   (encode-rest parsed-json 2 tab)))))

(defun encode-obj-arr (parsed-json type tab)
  (concatenate 'string
	       (if (= type 1) "{" "[")
	       (string #\Newline)
	       (make-string tab :initial-element #\Tab)
	       (jsonencode (rest parsed-json) type (+ tab 1))
	       (string #\Newline)
	       (make-string (- tab 1) :initial-element #\Tab)
	       (if (= type 1) "}" "]")))

(defun encode-obj (parsed-json tab)
  (concatenate 'string
	       "\""
	       (first (first parsed-json))
	       "\""
	       " : "
	       (get-element-obj parsed-json tab)))

(defun encode-rest (parsed-json type tab)
  (concatenate 'string
	       (funcall (if (= type 1)
			     #'encode-obj
			   #'get-element-arr) parsed-json tab)
	       ","
	       (string #\Newline)
	       (make-string (- tab 1) :initial-element #\Tab)
	       (jsonencode (rest parsed-json) type tab)))

;;; used to make the jsonencode code more readable

(defun get-element-obj (parsed-json tab) 
  (if (listp (second (first parsed-json))) 
      (jsonencode (second (first parsed-json)) 0 tab)
       (jsonencode (second (first parsed-json)) 0 tab)))

;;; used to make the jsonencode code more readable

(defun get-element-arr (parsed-json tab)
  (if (listp (first parsed-json))
      (jsonencode (first parsed-json) 0 tab)
      (write-to-string (jsonencode (first parsed-json) 0 tab))))

;;;; utility

(defun lis-str (l) (reduce (lambda (x y) (concatenate 'string x y)) l))

(defun append-e (l e) (if (null e) l (append l (list e))))

;;;; end of file -- jsonparse.lisp --
