;;;; -*- Mode: Lisp -*-
;;; -- jsonparse.lisp --


;;; --------------------------- jsonparse ----------------------------

(defun jsonparse (json-input)
  (if (stringp json-input)
      (json json-input)
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
  (json-element json-input))

(defun json-value (json-input)
  (or (json-obj json-input)
      (json-array json-input)
      (json-string json-input)
      (json-number json-input)
      (dcg-match json-input "true" :ret "true")
      (dcg-match json-input "false" :ret "false")
      (dcg-match json-input "null" :ret "null")))


(defun json-obj (json-input)
  (or (let ((f (dcg-and json-input
			(list "{" #'json-ws "}"))))
	(cond
	 ((car f)
	  (cons (car f) '(JSONOBJ)))))
      (let ((f (dcg-and json-input
			(list "{" #'json-ws #'json-members #'json-ws
			      "}"))))
	(cond
	 ((car f)
	  (cons (car f) (append '(JSONOBJ)
				(first (second f)))))))))


(defun json-element (json-input)
  (let ((f (dcg-and json-input
		    (list #'json-ws #'json-value #'json-ws))))
    (cond
     ((car f)
      (cons (car f) (first (second f)))))))

(defun json-members (x)
  NIL)

(defun json-string (json-input)
  (let ((f (dcg-and json-input
		    (list "\"" #'json-charachters "\""))))
    (cond
      ((car f)
      (cons (car f) (first (second f)))))))

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
      ))


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
				   (append acc
					   (and (second c) (list (second c))))
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

(defun dcg-match (json-input y &key ret)
  (let ((l (length y)))
    (cond
     ((>= (length json-input) l)
      (cond
       ((equal (subseq json-input 0 l) y)
	(list (subseq json-input l) (or ret y))))))))

(defun dcg-cut (json-input) (list json-input NIL T))

(defun dcg-or (x) (if (null (car x)) NIL x))

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
	(print res-string)
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
