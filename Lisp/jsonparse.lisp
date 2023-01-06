;;;; -*- Mode: Lisp -*-
;;; -- jsonparse.lisp --


;;; --------------------------- jsonparse ----------------------------

(defun jsonparse (json-input)
  (if (stringp json-input)
      (let ((j (json json-input)))
	(if (null j) (error "ERROR: syntax error") (second j)))
    (error "ERROR: json only accepts strings")))

(defun json-ws (json-input)
  (or (cond
       ((string= "" json-input)
	""))
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
      (dcg-match json-input "true" :ret "true")
      (dcg-match json-input "false" :ret "false")
      (dcg-match json-input "null" :ret "null")))


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
			    (flatten-l (first (second f))))))))


(defun json-element (json-input)
  (dcg-handle json-input
		  (list #'json-ws #'json-value #'json-ws)))

(defun json-members (json-input)
  (or (dcg-handle json-input
	      (list #'json-member "," #'dcg-cut #'json-members)
	      (lambda (f)
		(second f)))
      (dcg-handle json-input
		  (list #'json-member) (lambda (f)
		    (second f)))))

(defun json-member (json-input)
  (dcg-handle json-input
		  (list #'json-ws #'json-string #'json-ws ":"
			#'json-ws #'json-value #'json-ws)
		  (lambda (f)
		    (second f))))

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
			    (flatten-l (first (second f))))))))

(defun json-elements (json-input)
  (or (dcg-handle json-input
	      (list #'json-element "," #'dcg-cut #'json-elements)
	      (lambda (f)
		(second f)))
      (dcg-handle json-input
		  (list #'json-element) (lambda (f)
		    (second f)))))

(defun json-element (json-input)
  (dcg-handle json-input
		  (list #'json-ws #'json-value #'json-ws)
		  (lambda (f)
		    (first (second f)))))

;; ----- numbers ------

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
      (dcg-match e "")
      ))

(defun json-integer (json-input)
  (let ((m (dcg-match json-input "-")))
    (if (null m)
	(json-integer-h json-input)
	(dcg-handle (first m)
		  (list #'json-integer-h)
		  (lambda (f)
		    (concatenate 'string
			     "-"
			     (second f)))))))

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
	 ((< 48 (char-code f) 58) 
	  (list (subseq json-input 1) (string f)))))))

(defun json-digits (json-input)
  (or (dcg-handle json-input
		  (list #'json-digit #'json-digits)
		  (lambda (f)
		    (lis-str (second f))))
      (dcg-handle json-input
		  (list #'json-digit)
		  (lambda (f)
		    (lis-str (second f))))))

;; ----- strings ------

(defun json-string (json-input)
  (dcg-handle json-input
		  (list "\"" #'json-charachters "\"")))

(defun json-charachters (json-input)
  (let ((f (json-charachters-h json-input)))
    (list (car f) (coerce (reverse (cdr f)) 'string))))

(defun json-charachters-h (json-input)
  (or (let ((f (dcg-and json-input
			 (list "\\" #'dcg-cut #'json-escape ))))
	 (cond
	  ((car f)
	   (append (json-charachters-h (car f)) (second f)))
	  ((third f)
	   (list NIL))))
      (let ((f (dcg-and json-input
			 (list #'json-char))))
	 (cond
	  ((car f)
	   (append (json-charachters-h (car f)) (second f)))
	  ((third f)
	   (list NIL))))
      (list json-input)))

(defun json-char (json-input)
  (if (string= "" json-input)
      NIL
      (let ((f (char json-input 0)))
	(cond
	 ((char/= f #\") 
	  (list (subseq json-input 1) f))))))

(defun json-escape (e)
  (or (dcg-match e "\"" :ret #\")
      (dcg-match e "\\" :ret #\\)
      (dcg-match e "/" :ret #\/) 
      (dcg-match e "b" :ret #\Backspace) 
      (dcg-match e "f" :ret #\Page)     
      (dcg-match e "n" :ret #\Linefeed)
      (dcg-match e "r" :ret #\Return)
      (dcg-match e "t" :ret #\Tab)
      (dcg-match e "u" :ret #\u)
      ))

;;  ----------------- dcg stuff ------------------

(defun dcg-and (json-input &optional l acc cut)
  (if (null json-input)
      (list NIL NIL cut)
    (if (null l)
	(list json-input acc cut)
      (let ((f (first l)))
	(typecase f
	  (function (let ((c (funcall f json-input)))
		      (if (consp c)
			  (dcg-and (car c)
				   (rest l)
				   (append-e acc
					   (second c))
				   (or cut
				       (third c)))
			(dcg-and c
				 (rest l)
				 acc
				 cut))))
	  (string (dcg-and (car (dcg-match json-input f))
			   (rest l)
			   acc
			   cut)))))))

(defun dcg-match (json-input y &key ret ignore-case)
  (let ((l (length y)))
    (cond
     ((>= (length json-input) l)
      (let ((x (subseq json-input 0 l)))
	(cond
	 ((equal (if (null ignore-case)
		     x
		   (string-upcase x)) (if (null ignore-case)
		   y
		   (string-upcase y)))
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
	    (funcall fun f)))
     ((third f)
      (list NIL)))))

(defun dcg-cut (json-input) (list json-input NIL T))

;; ------ utility ------

(defun lis-str (l) (reduce (lambda (x y) (concatenate 'string x y)) l))

(defun append-e (l e) (if (null e) l (append l (list e))))

(defun flatten-l (l) (if (null l) nil (cons (car l) (flatten-l (second l)))))

;; (defun json-number (listch resList) 
;;   (cond ((and (> (char-code (first listch)) 47) (< (char-code (first listch)) 58)) 
;;          (cond ((or (null (second listch)) (eql (char-code (second listch)) 44) (eql (char-code (second listch)) 125) (eql (char-code (second listch)) 93)) 
;;                 (append resList (cons (first listch) nil)))
;;                (T (json-number (rest listch) (append resList (cons (first listch) nil))))))
;;         (T (error "Invalid number"))
;; ))


					;(defun value (listch resList)
					;  ((cond (eql (char-code (car listch)) 123) ((object (rest listch) resList))
					;         (eql (char-code (car listch)) 91) (array listch resList)
					;         (eql (char-code (car listch)) 34) (string listch resList)
					;         () ()
					;         () ()
					;         () ()
					;         () ()
					;         )))

;;; controllare che in { ws } ws NON sia vuoto se no si rompe char-code
					;(defun object (listch resList) 
					;  ((if (and (ws (first listch)) (eql (char-code (second listch)) 125)) 
					;          (append resList ('JSONOBJ))
					;       (and (members (rest listch)(append resList ('JSONOBJ))) 
					;          (eql (char-code (first listch)) 125))
					;)))

;;; ----- input e output -----

;;; jsonread/1
;;; jsonread(filename)
;;; read json file and print to screen for now
;;; call jsonparse where is the print in readfile

(defun jsonread (filename)
  (with-open-file (in filename :direction :input
		      :if-does-not-exist :error)
		  (readfile in "")))

(defun readfile (in res-string)
  (let ((s (read-char in nil 'eof)))
    (if (eq s 'eof)
	(jsonparse res-string)
      (readfile in
		(concatenate 'string
			     res-string
			     (string s))))))

;;;(readfile in (concatenate 'string res-string (list s))))))


;;; jsondump/1
;;; jsondump(JSON filename)
;;; write the JSON passed into the file specified by the path filename
;;; (IMPORTANT: ASK FOR EXAMPLE)
;;; need implement the revert conversion from jsonobj to true json

(defun jsondump (json filename)
  (with-open-file (out filename :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
		  (format out json)))

;;; end of file -- jsonparse.lisp --
