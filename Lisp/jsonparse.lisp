;;;; -*- Mode: Lisp -*-
;;; -- jsonparse.lisp --

;;; jsonparse/1
;;; jsonparse(JSONString)

;(defun jsonparse (JSONString) 
;  (json JSONString))

;(defun json (JSONString) (element JSONString resList))

(defun wsp (ch) 
  (cond ((eql (char-code ch) 32))
         ((eql (char-code ch) 10))
         ((eql (char-code ch) 13))
         ((eql (char-code ch) 9))
         ))
           ;(eql char-code(ch) 0) (T)
          
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
  (with-open-file (in 
                   filename 
                   :direction :input 
                   :if-does-not-exist :error)
   (readfile in "")))

(defun readfile (in res-string) 
  (let ((s (read-char in nil 'eof)))
     (if (eq s 'eof) (print res-string)
       (readfile in (concatenate 'string res-string (string s))))))
       ;;;(readfile in (concatenate 'string res-string (list s))))))


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


;(defun jsonencode (jsonobj s)
;  (cond ((string-equal (subseq jsonobj 0 9) "jsonobj([") (and (concatenate 'string s "{") (print (concatenate 'string "ciao" s)) (print jsonobj))) 
;        ((string-equal (subseq jsonobj 0 10) "jsonarray([") (concatenate 'string s "["))
;        ((else (and (concatenate 'string s) (print s) (print jsonobj))))
;))


;;; end of file -- jsonparse.lisp --
