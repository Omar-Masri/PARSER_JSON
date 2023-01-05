;;;; -*- Mode: Lisp -*-
;;; -- jsonparse.lisp --

;;; jsonparse/1
;;; jsonparse(JSONString)

;(defun jsonparse (JSONString) 
;  (json JSONString))

;(defun json (JSONString) (element JSONString resList))

(defun ws-p (ch) 
  (or (eql (char-code ch) 32)
         (eql (char-code ch) 10)
         (eql (char-code ch) 13)
         (eql (char-code ch) 9)
         ))

;;; resList diventa una lista con dentro tutti i caratteri della stringa
;;; (da convertire in stringa e appendere al risultato finale da chi l'ha chiamato)
(defun json-string (listch resList) 
  (cond ((null listch) (error "Invalid string"))
        ((eql (char-code (first listch)) 34) (append resList '(#\"))) 
        ((and (eql (char-code (first listch)) 92) (eql (char-code (second listch)) 34)) 
         (json-string (rest (rest listch)) (append resList '(#\\ #\"))))
        (T (json-string (rest listch) (append resList (cons (first listch) nil))))
       
))

(defun json-number (listch resList) 
  (cond ((and (> (char-code (first listch)) 47) (< (char-code (first listch)) 58)) 
         (cond ((or (null (second listch)) (eql (char-code (second listch)) 44) (eql (char-code (second listch)) 125) (eql (char-code (second listch)) 93)) 
                (append resList (cons (first listch) nil)))
               (T (json-number (rest listch) (append resList (cons (first listch) nil))))))
        (T (error "Invalid number"))
))

     
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

;;; ----- jsonaccess -----
(defun jsonaccess (json-obj &rest fields) 
  (cond ((eql (length json-obj) 1)(error "no match: empty json"))
        ((and (eql 'JSONOBJ (first json-obj)) (stringp (first fields)))
         (if (equal(first (second json-obj)) (first fields))
             (if (eql (length fields) 1) 
                 (second (second json-obj))
               (apply #'jsonaccess (second (second json-obj)) (rest fields)))
         (if (null (third json-obj)) 
             (error "no match: key not found") 
           (apply #'jsonaccess (remove (second json-obj) json-obj :count 1) fields))))
        ((and (eql 'JSONARRAY (first json-obj)) (integerp (first fields)) (>= (first fields) 0))
         (if (< (first fields) (- (length json-obj) 1)) 
             (if (= (length fields) 1) 
                 (nth (+ (first fields) 1) json-obj) 
               (apply #'jsonaccess (nth (+ (first fields) 1) json-obj) (rest fields))) 
           (error "no match: index out of bound")))
        (T (error "something went wrong!"))
))




;;; ----- input e output -----

;;; jsonread/1
;;; jsonread(filename)
;;; read json file and print to screen for now
;;; call jsonparse where is the print in readfile

(defun jsonread (filename)
  (with-open-file (in 
                   filename 
                   :direction :input 
                   :if-does-not-exist :error)
   (readfile in "")))

(defun readfile (in res-string) 
  (let ((s (read-line in nil 'eof)))
     (if (eq s 'eof) (print res-string)
       (readfile in (concatenate 'string res-string s (string #\Newline))))))


;;; jsondump/1
;;; jsondump(JSON filename)
;;; write the JSON passed into the file specified by the path filename
;;; (IMPORTANT: ASK FOR EXAMPLE)
;;; need implement the revert conversion from jsonobj to true json

(defun jsondump (json filename)
  (with-open-file (out 
                   filename
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
(format out json)))

;;;type:
; 0 neutral
; 1 jsonobj
; 2 jsonarray

(defun jsonencode (parsed-json type) 
  (cond ((zerop type)
             (cond ((not (listp parsed-json)) 
                     parsed-json)
                   ((eql 'JSONOBJ (first parsed-json))
                    (concatenate 'string "{" (string #\Newline) (jsonencode (rest parsed-json) 1) (string #\Newline) "}" (string #\Newline)))
                   ((eql 'JSONARRAY (first parsed-json))
                    (concatenate 'string "[" (string #\Newline) (jsonencode (rest parsed-json) 2) (string #\Newline) "]" (string #\Newline)))
                   ))
        ((= type 1) (if (null (rest parsed-json))
                        (concatenate 'string (string #\") (first (first parsed-json)) (string #\") " : " 
                         (get-element-obj parsed-json))
                      (concatenate 'string (string #\") (first (first parsed-json)) (string #\") " : " (get-element-obj parsed-json) "," (string #\Newline) (jsonencode (rest parsed-json) 1))))
        ((= type 2) (if (null (rest parsed-json)) 
                        (get-element-arr parsed-json)
                      (concatenate 'string (get-element-arr parsed-json) "," (string #\Newline) (jsonencode (rest parsed-json) 2))))
        
))

(defun get-element-obj (parsed-json) 
  (if (listp (second (first parsed-json))) 
      (jsonencode (second (first parsed-json)) 0) 
    (write-to-string (jsonencode (second (first parsed-json)) 0))))

(defun get-element-arr (parsed-json) 
  (if (listp (first parsed-json)) 
      (jsonencode (first parsed-json) 0) 
    (write-to-string (jsonencode (first parsed-json) 0))))
;;; end of file -- jsonparse.lisp --
