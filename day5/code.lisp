(ql:quickload :cl-ppcre)
(ql:quickload :cl-interval)

(defvar *input* nil)
(defstruct almanac seeds type-to-type type-to-ranges)
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

(defstruct segments
  interval-to-range
  intervals)

(defun read-input (stream)
  (let ((seeds (parse-seeds (read-line stream)))
        (maps (loop for block = (read-map-block stream) then (read-map-block stream)
                    while block collect block)))
    (make-almanac :seeds seeds
                  :type-to-type (let ((table (make-hash-table)))
                                  (loop for block in maps
                                        do (setf (gethash (car (map-block-header block)) table)
                                                 (cdr (map-block-header block))))
                                  table)
                  :type-to-ranges (let ((table (make-hash-table)))
                                    (loop
                                      for block in maps
                                      do (setf (gethash (car (map-block-header block)) table)
                                               (loop with interval-to-range = (make-hash-table :test 'equal)
                                                       with intervals = (interval:make-tree)
                                                     for range in (map-block-ranges block)
                                                     do (let ((interval (cons (range-s range) (1- (+ (range-l range) (range-s range)) ))))
                                                          (interval:insert intervals interval)
                                                          (setf (gethash interval interval-to-range ) range))
                                                     finally (return (make-segments :intervals intervals :interval-to-range interval-to-range)))))
                                    table))))


;; an item: value and type
(defstruct item v t)

(defun find-destination (item almanac)
  "for a type and a value, returns the destination type and value, using the maps in an almanac"
  (let* (
         (value (item-v item))
         (type (item-t item))
         (destination-type (gethash type (almanac-type-to-type almanac)))
         (segments (gethash type (almanac-type-to-ranges almanac))))
    (when destination-type
      (let* ((interval-all (interval:find-all (segments-intervals segments) value))
             (interval (when interval-all (first interval-all)))
             (range (when interval (gethash (cons (interval:interval-start interval) (interval:interval-end interval)) (segments-interval-to-range segments))))
             (destination-value (if range (+ (range-d range) (- value (range-s range))) value)))
        (make-item :v destination-value :t destination-type)))))


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

(find-destination (make-item :v 52 :t :SEED) *input*)

(solve *input*)

(defun solve2 (almanac)
  (loop for (s l) on (almanac-seeds almanac) :by #'cddr :while l
        do (print (cons s l))
        minimizing (loop for i from 0 below l
                         minimizing
                         (item-v (find-final-destination (make-item :v (+ s i) :t :SEED) almanac)))))

;;(load-input "test-input")

(load-input "input")

(time (solve2 *input*))

(let ((list '(1 2 3 4)))
  (loop :for (a b) :on list :by #'cddr :while b
        :collect (cons a b)))
