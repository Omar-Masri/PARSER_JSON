;;;; -*- Mode: Lisp -*-
;;; -- jsonparse.lisp --

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
       (readfile in (concatenate 'string res-string (list s))))))


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

;;; end of file -- jsonparse.lisp --
