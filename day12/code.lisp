(ql:quickload "cl-ppcre")
(ql:quickload "cl-ana.math-functions")
(use-package :cl-ana.math-functions)

(defstruct range start end)
(defun range(start end) (make-range :start start :end end))
;; k = known broken ranges, u = unknown ranges, s = sizes
(defstruct springs k u s)

(defmacro with-springs-vals ((springs k u s) &rest body)
  (let ((evaluated-spring-name (gensym)))
    `(let* (
            (,evaluated-spring-name ,springs)
            (,k (springs-k ,evaluated-spring-name))
            (,u (springs-u ,evaluated-spring-name))
            (,s (springs-s ,evaluated-spring-name)))
       ,@body)))

(defun range-size (range) (1+ (- (range-end range) (range-start range))))
(defun range-single (range) (= (range-size range) 1))
(defun ranges-sum (ranges) (loop for range in ranges summing (range-size range)))
(defun unknown-sum (springs) (ranges-sum (springs-u springs)))
(defun known-sum (springs) (ranges-sum (springs-k springs)))
(defun springs(k u s) (make-springs :k k :u u :s s))
(defun sizes-sum(springs) (reduce #'+ (springs-s springs)))
(defun missing-known(springs) (- (sizes-sum springs) (known-sum springs)))

(defun solution-p (known sizes)
  (or (and (not known) (not sizes))
      (and (= (range-size (car known)) (car sizes))
           (solution-p (cdr known) (cdr sizes)))))

(defun springs-solution-p (springs)
  (with-springs-vals (springs k u s)
    (and (= (length k) (length s))
         (solution-p
          (springs-k springs)
          (springs-s springs)))))

(defun read-springs (line)
  (let* ((parts (cl-ppcre:split " " line))
         (sizes (mapcar #'parse-integer (cl-ppcre:split "," (cadr parts)))))
    (loop with known = '()
          with unknown = '()
          with opening = nil
          for previous = #\. then current
          for current across (concatenate 'string (car parts) ".")
          for i = 0 then (1+ i)
          do (cond
               ((char= #\. previous) (when (not (eq current #\.)) (setf opening i)))
               ((char= #\# previous)
                (cond ((char= #\. current)
                       (push (range opening (1- i)) known))
                      ((char= #\? current)
                       (push (range opening (1- i)) known)
                       (setf opening i))))
               (t ; #\?
                (cond ((char= #\. current)
                       (push (range opening (1- i)) unknown))
                      ((char= #\# current)
                       (push (range opening (1- i)) unknown)
                       (setf opening i)))))
          finally (return (springs (reverse known) (reverse unknown) sizes)))))

(defun unknown-indices(springs)
  (loop for range in (springs-u springs)
        appending
        (loop for i from (range-start range) to (range-end range)
              collecting i)))

(defun count-solutions(line)
  (let* ((springs (read-springs line))
         (unknown-indices (unknown-indices springs))
         (missing-known (missing-known springs))
         (count 0))
    (for-combinations (index-combination (length unknown-indices) missing-known)
      (let ((candidate (copy-seq line))
            (indices (loop for i across index-combination collecting (nth i unknown-indices))))
        (loop for index in indices do (setf (aref candidate index) #\#))
        (when (springs-solution-p (read-springs candidate)) (incf count))))
    count))


(with-open-file (stream "input")
  (loop for line = (read-line stream nil)
        while line
        summing (count-solutions line)))
