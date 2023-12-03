(ql:quickload :cl-ppcre)

(defun read-lines (file)
  "return an array of lines (as strings ) from a file"
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect line into lines
          finally (return (make-array (length lines)
                                      :initial-contents lines)))))

(defun char-sym-p (c)
  "returns whether or not a character is a symbol according to the problem definition"
  (and (not (char= c #\.)) (not (digit-char-p c))))

(defvar *input* (read-lines "input-real"))
;;(setf *input* (read-lines "input-real"))
(defun get-input-char (line col)
  "returns the character at the given line and column in *input*"
  (char  (aref *input* line) col))

(defstruct h-number-segment
  "a struct containing a number segment as the start and end indices in a line (inclusive) and its value"
  start
  end
  value)

(defstruct number-segment
  "a struct containing a number segment as the start and end in a line, and the line index, and the number value"
  line
  start
  end
  value)


(defun extract-line-number-segments (line)
  "extract the number segments from an input line"
  (let ((ns (cl-ppcre:all-matches "\\d+" line)))
    (loop for (start end) on ns by #'cddr while end
          collect (make-h-number-segment :start start :end (1- end) :value (parse-integer (subseq line start end))))))

(defun extract-number-segments ()
  "extract the number segments from *input*"
  (loop for line across *input*
        for i = 0 then (1+ i)
        appending (let ((nss (extract-line-number-segments line)))
                    (loop for ns in nss collecting
                          (make-number-segment
                           :line i
                           :start (h-number-segment-start ns)
                           :end (h-number-segment-end ns)
                           :value (h-number-segment-value ns))))))

(defun in-border-p (line col ns)
  "given a position (line, col), returns whether or not it is in the border of a number segment"
  (or
                                        ; on top border
   (and (= line (1- (number-segment-line ns)))
        (>= col (1- (number-segment-start ns)))
        (<= col (1+ (number-segment-end ns))))

                                        ; on bottom border
   (and (= line (1+ (number-segment-line ns)))
        (>= col (1- (number-segment-start ns)))
        (<= col (1+ (number-segment-end ns))))

                                        ; on left border
   (and (= line (number-segment-line ns))
        (= col (1- (number-segment-start ns))))

                                        ; on left border
   (and (= line (number-segment-line ns))
        (= col (1+ (number-segment-end ns))))))

;; part1
(loop for line from 0 upto (1- (length *input*))
      with valid-nss = '()
      with nss = (extract-number-segments)
      do (loop
           for col from 0 upto (1- (length (aref *input* line)))
           do (when (char-sym-p (get-input-char line col))
                (loop for ns in nss
                      with marked-for-deletion = '()
                      do (when (in-border-p line col ns)
                           (print ns)
                           (push ns valid-nss)
                           (push ns marked-for-deletion))
                      finally (setf nss
                                    (remove-if
                                     #'(lambda (x) (member x marked-for-deletion
                                                           :test #'equal)) nss))))
           )
      finally (return (loop for ns in valid-nss summing (number-segment-value ns))))

;; part2
(loop for line from 0 upto (1- (length *input*))
      with nss = (extract-number-segments)
      summing (loop
           for col from 0 upto (1- (length (aref *input* line)))
           summing (if (char= #\* (get-input-char line col))
                       (let ((adjacents (loop for ns in nss
                                              when (in-border-p line col ns) collect ns)))
                         (if (= 2 (length adjacents))
                             (* (number-segment-value (first adjacents))
                                (number-segment-value (second adjacents)))
                             0)
                         ) 0)))
