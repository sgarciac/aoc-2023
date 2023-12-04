(ql:quickload :cl-ppcre)

(defun parse-line (line)
  (let ((halfs (cl-ppcre:split "\\|" (cadr (cl-ppcre:split ":" line)))))
    (cons (mapcar #'parse-integer (remove "" (cl-ppcre:split " " (car halfs) :with-registers-p t) :test #'string=))
          (mapcar #'parse-integer (remove "" (cl-ppcre:split " " (cadr halfs) :with-registers-p t)  :test #'string=)))))

(defun read-input (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))

(defun points (parsed-line)
  (let ((common-count (length (intersection (car parsed-line) (cdr parsed-line)))))
    (if (zerop common-count) 0 (expt  2 (1- common-count)))))

(loop for entry in (read-input "input")
      summing (points entry))
