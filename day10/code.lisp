(defvar *map*)
(defvar *rows-count*)
(defvar *cols-count*)
(defvar *start*)
;; N E S W
(defun tc(char) (case char (#\. :GROUND) (#\S :START) (#\L :NE) (#\| :NS) (#\J :NW) (#\F :ES) (#\- :EW) (#\7 :SW)))

(defun find-start()
  (loop for row from 0 below *rows-count*
        do (loop for col from 0 below *cols-count*
                 do (when (eq (aref *map* row col) :START) (return-from
                                                            find-start (cons row col))))))

(defun cell(row col) (cons row col))
(defun row(cell) (car cell))
(defun col(cell) (cdr cell))

(defun val(cell) (unless (or (>= (row cell) *rows-count*) (>= (col cell) *cols-count*)
                             (< (row cell) 0)
                             (< (col cell) 0))
                   (aref *map* (row cell) (col cell))))

(defun is-pipe(cell) (and cell (val cell) (not (eq (val cell) :GROUND))))
(defun north (cell) (when (> (row cell) 0) (cell (1- (row cell)) (col cell))))
(defun south (cell) (when (< (row cell) (1- *rows-count*)) (cell (1+ (row cell)) (col cell))))
(defun east (cell) (when (< (col cell) (1- *cols-count*)) (cell (row cell) (1+ (col cell)))))
(defun west (cell) (when (> (col cell) 0) (cell (row cell) (1- (col cell)))))
(defun go-to (cell dir) (case dir (:N (north cell)) (:S (south cell)) (:E (east cell)) (:W (west cell))))
(defun opdir (dir) (case dir (:N :S) (:E :W) (:S :N) (:W :E)))
(defun dirs (cell) (case (val cell) (:NE '(:N :E)) (:NS '(:N :S)) (:NW '(:N :W)) (:ES '(:E :S)) (:EW '(:E :W)) (:SW '(:S :W))))

(defun next-dir (cell from)
  (let ((dirs (dirs cell)))
    (car (remove from dirs))))

(defun find-start-pipe()
  (let ((north (north *start*))
        (east (east *start*))
        (south (south *start*))
        (west (west *start*)))
    (cond ((and (is-pipe north) (find :S (dirs north))
                (is-pipe east) (find :W (dirs east))) :NE)
          ((and (is-pipe north) (find :S (dirs north))
                (is-pipe south) (find :N (dirs south))) :NS)
          ((and (is-pipe north) (find :S (dirs north))
                (is-pipe west) (find :E (dirs west))) :NW)
          ((and (is-pipe east) (find :W (dirs east))
                (is-pipe south) (find :N (dirs south))) :ES)
          ((and (is-pipe east) (find :W (dirs east))
                (is-pipe west) (find :E (dirs west))) :EW)
          ((and (is-pipe south) (find :N (dirs south))
                (is-pipe west) (find :E (dirs west))) :SW))))

(defun load-input (file)
  (setf *map*
        (let ((lines (with-open-file (stream file)
                       (loop for line = (read-line stream nil)
                             while line
                             collecting line))))
          (loop
            with map = (make-array `(,(length lines) ,(length (car lines))))
            for row = 0 then (1+ row)
            for line in lines
            do (loop for col = 0 then (1+ col)
                     for char across line
                     do (setf (aref map row col) (tc char))
                     )
            finally (return map)
            )))

  (destructuring-bind (rows-count cols-count) (array-dimensions *map*)
    (setf *rows-count* rows-count *cols-count* cols-count))
  (setf *start* (find-start))
  (print *start*)
  (setf (aref *map* (row *start*) (col *start*)) (find-start-pipe)))

;; part 1
(load-input "input")

(/ (loop
     for current = *start* then (go-to current direction)
     for direction = (next-dir *start* nil) then (next-dir current (opdir direction))
     for i = 0 then (1+ i)
     while (or (zerop i) (not (equal current *start*)))
     finally (return i)) 2)

;; Part 2
(defvar *paths*)

(defun load-path(file)
  (setf *paths* (make-hash-table :test #'equal))
  (load-input file)
  (let ((steps (loop
                 for current = *start* then (go-to current direction)
                 for direction = (next-dir *start* nil) then (next-dir current (opdir direction))
                 for i = 0 then (1+ i)
                 while (or (zerop i) (not (equal current *start*)))
                 collecting current)))
    (loop for step in steps do (setf (gethash step *paths*) t))))

(defun in-path-p (cell) (gethash cell *paths*))

(load-path "input")

(defun inside-p (cell)
  (if (in-path-p cell)
      nil
      (loop
        with current-coin = nil
        with diagonal-crossings = 0
        for col from (1+ (col cell)) below *cols-count*
        counting (and (in-path-p (cell (row cell) col))(eq (val (cell (row cell) col)) :NS)) into straight-crossings
        do (when (in-path-p (cell (row cell) col))
             (cond ((and (eq (val (cell (row cell) col)) :ES) (not current-coin))
                    (setf current-coin :ES))
                   ((and (eq (val (cell (row cell) col)) :NW) (eq current-coin :ES))
                    (incf diagonal-crossings)
                    (setf current-coin nil))
                   ((and (eq (val (cell (row cell) col)) :SW) (eq current-coin :ES))
                    (setf current-coin nil))
                   ((and (eq (val (cell (row cell) col)) :NE) (not current-coin))
                    (setf current-coin :NE))
                   ((and (eq (val (cell (row cell) col)) :SW) (eq current-coin :NE))
                    (incf diagonal-crossings)
                    (setf current-coin nil))
                   ((and (eq (val (cell (row cell) col)) :NW) (eq current-coin :NE))
                    (setf current-coin nil))))
        finally (return (oddp (+ straight-crossings diagonal-crossings))))))

(loop for row from 0 below *rows-count*
      do (print "")
      summing (loop
                for col from 0 below *cols-count*
                do (princ (cond ((inside-p (cell row col)) #\I) ((in-path-p (cell row col)) #\*) (t #\ )))
                counting (inside-p (cell row col))))
