(defun cell(row col) (cons row col))
(defun row(cell) (car cell))
(defun col(cell) (cdr cell))

(defun read-input (file)
  (with-open-file (stream file)
    (loop for l = (read-line stream nil)
          for row = 0 then (1+ row)
          while l
          appending (loop
               for c across l
               for col = 0 then (1+ col)
               when (char= #\# c) collect (cell row col)))))

(defun get-rows(cells) (sort (mapcar #'row cells) #'<))
(defun get-cols(cells) (sort (mapcar #'col cells) #'<))

(defun make-set(numbers)
  (let ((set (make-hash-table)))
    (loop for number in numbers do (setf (gethash number set) t))
    set))

(defun in-set(item set) (gethash item set))

(defun empty-rows-between(cell1 cell2 rows-set)
  (let ((max (max (row cell1) (row cell2)))
        (min (min (row cell1) (row cell2))))
    (loop for row from min upto max
          counting (not (in-set row rows-set)))))

(defun empty-cols-between(cell1 cell2 cols-set)
  (let ((max (max (col cell1) (col cell2)))
        (min (min (col cell1) (col cell2))))
    (loop for col from min upto max
          counting (not (in-set col cols-set)))))

(defun distance(cell1 cell2 expansion rows-set cols-set)
  (let* ((simple-y-distance (abs (- (row cell1) (row cell2))))
         (simple-x-distance (abs (- (col cell1) (col cell2))))
         (empty-rows (empty-rows-between cell1 cell2 rows-set))
         (empty-cols (empty-cols-between cell1 cell2 cols-set)))
    (+
     (+ (- simple-y-distance empty-rows)
        (* expansion empty-rows))
     (+ (- simple-x-distance empty-cols)
        (* expansion empty-cols)))))

(defun solve (expansion)
  (/ (let* ((input (read-input "input"))
            (rows (get-rows input))
            (cols (get-cols input))
            (rows-set (make-set rows))
            (cols-set (make-set cols)))
       (loop for p1 in input
             summing (loop for p2 in input summing (distance  p1 p2 expansion rows-set cols-set)))) 2))

;;part1
(solve 2)
;;part2
(solve 1000000)
