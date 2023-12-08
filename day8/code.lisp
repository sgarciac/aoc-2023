(ql:quickload "cl-ppcre")
(use-package :cl-ppcre)

(defvar *instructions*) ;;
(defvar *nodes*)
(defvar *left-map*)
(defvar *right-map*)

(defun load-input (file)
  (with-open-file (stream file)
    (loop for char across (read-line stream)
          collecting (intern (string char)) into instructions
          finally (setf *instructions*
                        (make-array (length instructions)
                                    :initial-contents
                                    instructions)))
    (read-line stream) ;; empty line
    (loop
      with left-map = (make-hash-table)
      with right-map = (make-hash-table)
      with nodes = '()
      for line = (read-line stream nil)
      while line
      do (let ((origin (intern (subseq line 0 3)))
               (left (intern (subseq line 7 10)))
               (right (intern (subseq line 12 15))))
           (setf nodes (adjoin origin nodes))
           (setf nodes (adjoin left nodes))
           (setf nodes (adjoin right nodes))
           (setf (gethash origin left-map) left)
           (setf (gethash origin right-map) right))
      finally (progn
                (setf *left-map* left-map)
                (setf *right-map* right-map)
                (setf *nodes* nodes)))))

(defun get-left (origin) (gethash origin *left-map*))
(defun get-right (origin) (gethash origin *right-map*))

(defun get-next (origin instruction)
  (if (eq instruction 'L)
      (get-left origin)
      (get-right origin)))

;; part1
(load-input "input")

(loop
  for i = 0 then (1+ i)
  for node = 'AAA then (get-next node instruction)
  for instruction = (aref *instructions* (mod i (length *instructions*))) then (aref *instructions* (mod i (length *instructions*)))
  while (not (eq node 'ZZZ))
  finally (return i))

;; part 2
(defun get-starting-points()
  (loop for node in *nodes* when (char= #\A (char (string node) 2)) collect node))

(defun is-end-point(node)
  (char= #\Z (char (string node) 2)))

(apply #'lcm
       (loop for sp in (get-starting-points)
             collecting
             (loop
               for i = 0 then (1+ i)
               for node = sp then (get-next node instruction)
               for instruction = (aref *instructions* (mod i (length *instructions*))) then (aref *instructions* (mod i (length *instructions*)))
               while  (not (is-end-point node))
               finally (return i))))
