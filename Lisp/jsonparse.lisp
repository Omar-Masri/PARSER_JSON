;;;; -*- Mode: Lisp -*-
;;; -- jsonparse.lisp --

;;; ----- input e output -----

(defun jsonread (filename)
  (with-open-file (in 
                   filename 
                   :direction :input 
                   :if-does-not-exist :error)
   (readfile in "")))

(defun readfile (in res-string) 
  (let ((s (read-char in nil 'eof)))
     (if (eq s 'eof) (print res-string) (readfile in (concatenate 'string res-string (list s))))))

(defun jsondump (json filename)
  (with-open-file (out 
                   filename
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
(format out json)))

;;; end of file -- jsonparse.lisp --
