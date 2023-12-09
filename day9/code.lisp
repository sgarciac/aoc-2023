;; examplle:
;;
;; 1  3  6  10  15  21   -> 28
;; 0  2  3   4   5   6   -> 7
;; 0  0  1   1   1   1   -> 1
;; 0  0  0   0   0   0   -> 0
;;
;; initial = [v1, v2, v3 ....]
;;
;; if col < len(initial):
;;
;;   f(c,0) = initial[i]
;;   f(c,r) = if c<=r then 0, else f(c, r - 1) - f(c - 1, r - 2)
;;
;; if col >= len(initial)
;;
;;   f(c,r) = if r > len(initial) then 0 else f(c, r + 1) + f(c - 1, r)


(ql:quickload "cl-ppcre")

(defun f(initial col row)
  (if (< col (length initial))
      (if (zerop row)
          (aref initial col)
          (if (<= col row)
              0
              (- (f initial col (1- row)) (f initial (1- col) (1- row)))))
      (if (>= row (length initial))
          0
          (+ (f initial col (1+ row)) (f initial (1- col) row)))))

;;; part 1
(with-open-file (stream "input")
  (loop
    for line = (read-line stream nil)
    while line
    summing (let* ((parts (cl-ppcre:split " " line))
                   (initial (make-array (length parts) :initial-contents (mapcar #'parse-integer parts))))
              (f initial (length initial) 0))))

;;part 2
(with-open-file (stream "input")
  (loop
    for line = (read-line stream nil)
    while line
    summing (let* ((parts (reverse (cl-ppcre:split " " line)))
                   (initial (make-array (length parts) :initial-contents (mapcar #'parse-integer parts))))
              (f initial (length initial) 0))))
