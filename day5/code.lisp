(ql:quickload :cl-ppcre)

(defvar *input* nil)
(defstruct almanac seeds maps)
(defun tokey (string) (intern (string-upcase string) "KEYWORD"))
(defun parse-seeds (line) (mapcar #'parse-integer (cl-ppcre:split " " (subseq line 7))))
(defun parse-map-header (line) (let ((name (cl-ppcre:split "-" (car (cl-ppcre:split " " line))))) (cons (tokey (first name)) (tokey (third name)))))
(defstruct range s d l)
(defun parse-range (line) (let ((numbers (mapcar #'parse-integer (cl-ppcre:split " " line)))) (make-range :d (first numbers) :s (second numbers) :l (third numbers))))
(defstruct map-block header ranges)

(defun read-map-block (stream)
  (when (peek-char t stream nil nil)
    (let ((header (parse-map-header (read-line stream)))
          (ranges (loop for line = (read-line stream nil)
                        while (and line (not (zerop (length line))))
                        collect (parse-range line))))
      (make-map-block :header header :ranges ranges))))

(defun read-input (stream)
  (make-almanac :seeds (parse-seeds (read-line stream))
                :maps
                (loop for block = (read-map-block stream) then (read-map-block stream)
                            while block collect block)))
;; an item: value and type
(defstruct item v t)

(defun find-destination (item almanac)
  "for a type and a value, returns the destination type and value, using the maps in an almanac"
  (let* (
         (value (item-v item))
         (type (item-t item))
         (map (find-if (lambda (b) (eq type (car (map-block-header b)))) (almanac-maps almanac))))
    (when map (let* ((range (find-if (lambda (r) (and (>= value (range-s r)) (< value (+ (range-l r)(range-s r))))) (map-block-ranges map)))
                   (destination-value (if range (+ (range-d range) (- value (range-s range))) value)))
              (make-item :v destination-value :t (cdr (map-block-header map)))))))

(defun find-final-destination (item almanac)
  (let ((destination (find-destination item almanac)))
    (if destination
        (find-final-destination destination  almanac)
        item)))

(defun load-input (file) (setf *input* (with-open-file (stream  file) (read-input stream))))

(defun solve (almanac)
  (loop for seed in (almanac-seeds almanac)
        minimizing (item-v (find-final-destination (make-item :v seed :t :SEED) almanac))))

;;(load-input "test-input")
(load-input "input")

(solve *input*)

(defun solve2 (almanac)
  (loop for (s l) on (almanac-seeds almanac) :by #'cddr :while l
        do (print (cons s l))
        minimizing (loop for i from 0 below l
                         minimizing
                         (item-v (find-final-destination (make-item :v (+ s i) :t :SEED) almanac)))))



(load-input "test-input")
(load-input "input")

(solve2 *input*)

(let ((list '(1 2 3 4)))
  (loop :for (a b) :on list :by #'cddr :while b
        :collect (cons a b)))
