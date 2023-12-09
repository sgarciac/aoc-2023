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

(defun read-input(file)
  (with-open-file (stream file)
    (loop
      for l = (read-line stream nil)
      while l
      collecting (let* ((parts (cl-ppcre:split " " l)))
                   (make-array (length parts) :initial-contents (mapcar #'parse-integer parts))))))

;; part 1
(loop for vector in (read-input "input") summing (f vector (length vector) 0))

;; part 2
(loop for vector in (read-input "input") summing (f (reverse vector) (length vector) 0))
