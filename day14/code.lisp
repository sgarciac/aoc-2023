(ql:quickload :cl-ppcre)

(defun read-lines (file)
  "return an array of lines (as strings ) from a file"
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line collect line)))



(defun to-east (lines) lines)
(defun to-west (lines) (mapcar #'reverse lines))

(defun to-north (group)
  (loop for i from 0 below (length (first group))
        collect (reverse (coerce (mapcar (lambda (line) (aref line i)) group) 'string))))

(defun to-south (group)
  (loop for i from 0 below (length (first group))
        collect (coerce (mapcar (lambda (line) (aref line i)) group) 'string)))

(defun summatory(n)
  (/ (* n (1+ n))  2))

(defun score-line (line)
  (loop with segments = (cl-ppcre:split "#" line)
        for segment in segments
        and previous-lines-count = 0 then (+ 1 previous-lines-count (length segment))
        summing (let* ((total (length segment))
                       (rounded (count #\O segment))
                       (highlimit (+ previous-lines-count total))
                       (lowlimit (+ previous-lines-count (- total rounded))))
                  (- (summatory highlimit) (summatory lowlimit)))))

(defun score-lines (lines)
  (loop for line in lines
        summing (score-line line)))
;; part1
(score-lines (to-north (read-lines "input")))

;; part2
(defun without-last(l)
  (reverse (cdr (reverse l))))

(defun tilt (line)
  (loop with segments = (cl-ppcre:split "#" line :limit 10000)
        for segment in segments
        collecting (let* ((total (length segment))
                          (empty (count #\. segment))
                          (new-segment (make-string total :initial-element #\.)))
                     (loop for i from (1- total) downto empty
                           do (setf (aref new-segment i) #\O))
                     new-segment) into new-segments
        finally (return (format nil "~{~A~^#~}" new-segments))
        ))

(defun tilt-lines (lines)
  (mapcar #'tilt lines))

(defun cycle (lines)
  (tilt-lines (to-north (tilt-lines (to-north (tilt-lines (to-north (tilt-lines (to-north lines)))))))))

(let ((cycles (multiple-value-bind (conf second)
                  (block myloop
                    (loop
                      with memo = (make-hash-table :test #'equal)
                      for current = (read-lines "input") then (cycle current)
                      for i from 0 upto 2000
                      do (if (gethash current memo) (return-from myloop (values current i)) (setf (gethash current memo) current))))
                (let ((first (block myloop2
                               (loop
                                 for current = (read-lines "input") then (cycle current)
                                 for i from 0 upto 1000
                                 do (if (equal current conf) (return-from myloop2 i) )))))
                  (let ((cycle-size (- second first)))
                    (+ first (mod (- 1000000000 first) cycle-size))
                    )))))
  (loop for current = (read-lines "input") then (cycle current)
        for i from 0 below cycles
        finally (return (loop for i = (length current) then (1- i)
                              for line in current
                              summing (* i (count #\O line))
                              ))))
