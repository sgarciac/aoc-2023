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

;; consume methods return whether or not the consumption is possible, and the items left if possible AND the number of consumed unknown
(defun consume-good-or-unknown (items count unknown-consumed)
  (cond ((zerop count) (values t items unknown-consumed))
        ((not items) (values nil nil nil))
        ((good-p (car items)) (consume-good-or-unknown (cdr items) (1- count) unknown-consumed))
        ((unknown-p (car items)) (consume-good-or-unknown (cdr items) (1- count) (1+ unknown-consumed)))
        (t (values nil nil nil))))

(defun consume-broken-or-unknown (items count unknown-consumed)
  (cond ((zerop count) (values t items unknown-consumed))
        ((not items) (values nil nil nil))
        ((broken-p (car items)) (consume-broken-or-unknown (cdr items) (1- count) unknown-consumed))
        ((unknown-p (car items)) (consume-broken-or-unknown (cdr items) (1- count) (1+ unknown-consumed)))
        (t (values nil nil nil))))

(defun memo-key (items sizes) (format nil "~A ~A" items sizes))

(defun count-solutions-helper (items sizes missing unknown-left memo)
  (cond
    ((gethash (memo-key items sizes) memo) (gethash (memo-key items sizes) memo))
    ((and (not sizes) (zerop missing))  1)
    ((not sizes) 0)
    ((not (<= missing unknown-left)) 0)
    (t (let ((result (multiple-value-bind (result-space left-space unknown-consumed-space)
                         (consume-good-or-unknown items 1 0)
                       (if result-space ;; can consume the separator
                           (+
                            (multiple-value-bind (result left unknown-consumed)
                                (consume-broken-or-unknown left-space (car sizes) 0)
                              (if result
                                  (count-solutions-helper left (cdr sizes) (- missing unknown-consumed) (- unknown-left (+ unknown-consumed-space unknown-consumed)) memo)
                                  0))
                            (count-solutions-helper (cdr items) sizes missing (- unknown-left unknown-consumed-space) memo))
                           0
                           ))))
         (setf (gethash (memo-key items sizes) memo) result)
         result
         ))))

(defun count-solutions (items sizes)
  (count-solutions-helper items sizes (- (reduce #'+ sizes) (count #\# items)) (count #\? items)
                          (make-hash-table :test #'equal)))


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
