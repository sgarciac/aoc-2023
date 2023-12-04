(ql:quickload :cl-ppcre)

(defun points (parsed-line)
  (let ((common-count (length (intersection (car parsed-line) (cdr parsed-line)))))
    (if (zerop common-count) 0 (expt  2 (1- common-count)))))

(defstruct card pos mp copies)

(defun parse-line (line position)
  (let* ((halfs (cl-ppcre:split "\\|" (cadr (cl-ppcre:split ":" line))))
         (ws (mapcar #'parse-integer (remove "" (cl-ppcre:split " " (car halfs) :with-registers-p t) :test #'string=)))
         (ns (mapcar #'parse-integer (remove "" (cl-ppcre:split " " (cadr halfs) :with-registers-p t)  :test #'string=)))
         (mp (length (intersection ws ns))))
    (make-card :mp mp
               :pos position
               :copies 1
               )))

(defun read-input (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          for position = 1 then (1+ position)
          while line
          collect (parse-line line position) into parsed-lines
          finally (return (make-array (length parsed-lines) :initial-contents parsed-lines))
          )))

(loop
  with input = (read-input "input")
  for i from 0 upto (1- (length input))
do (let ((card (aref input i)))
     (dotimes (j (card-copies card))
       (loop
         for k from (+ i 1) upto (min (1- (length input)) (+ i (card-mp card)))
         do (when (< k (length input))
              (incf (card-copies (aref input k)))))))
  finally (return (loop for card across input summing (card-copies card))))
