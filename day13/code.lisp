(ql:quickload "split-sequence")


(defun read-entries (file)
  (split-sequence:split-sequence
   ""
   (with-open-file (stream file)
     (loop for line = (read-line stream nil)
                                   while line
           collect line))
   :test #'equal
   ))

(defun entries-to-horizontal (group)
  (coerce (mapcar #'intern group) 'vector))

(defun entries-to-vertical (group)
  (coerce (loop for i from 0 below (length (first group))
                collect (intern (coerce (mapcar (lambda (line) (aref line i)) group) 'string))) 'vector))

(defun mirror-p (entries i)
  (loop for j from 1 upto (min i (- (length entries) i))
        do (unless (eq (aref entries (- i j)) (aref entries (1- (+ i j))))
             (return-from mirror-p nil))) t)

(defun find-mirror-lines (entries)
  (loop for i from 1 below (length entries)
        when (mirror-p entries i) collect i))

(defun score-1 (group)
  (let ((mvs (find-mirror-lines (entries-to-vertical group))))
    (if mvs
        (first mvs)
        (let ((mhs (find-mirror-lines (entries-to-horizontal group))))
          (when mhs (* 100 (first mhs)))))))

(defun swap (group row col)
  (loop
    for i = 0 then (1+ i)
    for line in group collect (if (= i row)
                                  (let ((new-line (copy-seq line)))
                                    (setf (aref new-line col) (if (char= (aref line col) #\.) #\# #\.))
                                    new-line)
                                  line)))

(defun score-2 (group)
  (let ((mvs-original (find-mirror-lines (entries-to-vertical group)))
        (mhs-original (find-mirror-lines (entries-to-horizontal group))))
    (loop
      for row from 0 below (length group)
      do (loop for col from 0 below (length (first group))
               do (let ((swapped (swap group row col)))
                    (let
                        ((mvs-swap (remove (and mvs-original (first mvs-original)) (find-mirror-lines (entries-to-vertical swapped))))
                         (mhs-swap (remove (and mhs-original (first mhs-original)) (find-mirror-lines (entries-to-horizontal swapped)))))
                      (if mvs-swap
                          (return-from score-2 (first mvs-swap))
                          (when (first mhs-swap) (return-from score-2 (* 100 (first mhs-swap)))))))))))

;; part 1
(time (loop for group in (read-entries "input") summing (score-1 group)))

;; part 2
(time (loop for group in (read-entries "input") summing (score-2 group)))
