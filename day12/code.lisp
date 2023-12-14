(ql:quickload "cl-ppcre")

(defun items (springs) (car items))
(defun sizes (springs) (cdr items))

(defun broken-p (c) (char= #\# c))
(defun unknown-p (c) (char= #\? c))
(defun good-p (c) (char= #\. c))
(defun broken-or-unknown-p (c) (or (broken-p c) (unknown-p c)))
(defun good-or-unknown-p (c) (or (good-p c) (unknown-p c)))

(defun read-springs (line)
  (let* ((parts (cl-ppcre:split " " line))
         (sizes (mapcar #'parse-integer (cl-ppcre:split "," (cadr parts)))))
    (values (loop for c across (car parts) collect c) sizes)))



;; consume methods return whether or not the consumption is possible, and the items left if possible
(defun consume-good-or-unknown (items count)
  (cond ((zerop count) (values t items))
        ((not items) (values nil nil))
        ((not (good-or-unknown-p (car items))) (values nil nil))
        (t (consume-good-or-unknown (cdr items) (1- count)))))

(defun consume-broken-or-unknown (items count)
  (cond ((zerop count) (values t items))
        ((not items) (values nil nil))
        ((not (broken-or-unknown-p (car items))) (values nil nil))
        (t (consume-broken-or-unknown (cdr items) (1- count)))))

(defun still-valid (items sizes)
  (loop for )
  )

(defun count-solutions (items sizes)
  (if (not sizes) 1
                    (multiple-value-bind (result-space left-space) (consume-good-or-unknown items 1)
                      (if result-space ;; can consume the separator
                          (+
                           (multiple-value-bind (result left) (consume-broken-or-unknown left-space (car sizes))
                             (if result
                                 (count-solutions left (cdr sizes))
                                 0))
                           (count-solutions (cdr items) sizes))
                          0
                          ))))


(defun mult_sizes (l) (concatenate 'list (copy-seq l) (copy-seq l) (copy-seq l) (copy-seq l) (copy-seq l)))
(defun mult_template (l) (concatenate 'list (copy-seq l) '(#\?) (copy-seq l) '(#\?) (copy-seq l) '(#\?) (copy-seq l) '(#\?) (copy-seq l)))

(multiple-value-bind (items sizes) (read-springs "???.### 1,1,3")
  (count-solutions (cons #\. (mult_template items)) (mult_sizes sizes)))

(multiple-value-bind (items sizes) (read-springs "???.### 1,1,3")
  (count-solutions (cons #\. items) sizes))

(list_x_5 '(1 2))

(time (with-open-file (stream "input")
        (loop for line = (read-line stream nil)
              while line
              do (format t "~A => ~A~%" line (multiple-value-bind (items sizes) (read-springs line) (count-solutions
                                                                                                     (cons #\.
                                                                                                           items)
                                                                                                     sizes)))
              summing (multiple-value-bind (items sizes) (read-springs line) (count-solutions
                                                                              (cons #\.
                                                                                    items)
                                                                              sizes)))))

;; part2
(time (with-open-file (stream "input")
        (loop for line = (read-line stream nil)
              while line
              do (format t "~A => ~A~%" line (multiple-value-bind (items sizes) (read-springs line) (count-solutions
                                                                                                     (cons #\.
                                                                                                           (mult_template items))
                                                                                                     (mult_sizes sizes))))
              summing (multiple-value-bind (items sizes) (read-springs line) (count-solutions
                                                                              (cons #\.
                                                                                    (mult_template items))
                                                                              (mult_sizes sizes))))))
