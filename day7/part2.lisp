(ql:quickload "cl-ppcre")
(use-package :cl-ppcre)


(defun tc(c) (case c (#\T #\A) (#\J #\1) (#\Q #\C) (#\K #\D) (#\A #\E) (t c)))
(defun ts(s) (loop for c across s collect (tc c) into d finally (return (concatenate 'string d))))

(defun group-cards (cards)
  (loop
    with counts = '()
    for char across cards
    do (let ((group (assoc char counts :test #'char=)))
         (if group
             (incf (cdr group))
             (push (cons char 1) counts)))
    finally (return counts)))


(defun calculate-hand-type(cards)
  (let* ((groups (group-cards cards))
         (groups-count (length groups))
         (largest (loop with l = (first groups)
                        for group in groups
                        do (when (> (cdr group) (cdr l))
                             (setf l group))
                        finally (return l))))
    (cond ((= 1 groups-count) 6)
          ((= 2 groups-count)
           (if (= 4 (cdr largest)) 5 4))
          ((= 3 groups-count)
           (if (= 3 (cdr largest)) 3 2))
          ((= 4 groups-count) 1)
          (t 0))))

(defun joker-calculate-hand-type(cards)
  (loop for r across "ACDE987654321"
        maximizing (calculate-hand-type (regex-replace-all "1" cards (string r)))))


(defstruct hand cards type)
(defstruct bet hand bid)
(defvar *input*)

(defun bet< (bet1 bet2)
  (or (<
       (hand-type (bet-hand bet1))
       (hand-type (bet-hand bet2)))
      (and
       (=
        (hand-type (bet-hand bet1))
        (hand-type (bet-hand bet2)))
       (string<
        (hand-cards (bet-hand bet1))
        (hand-cards (bet-hand bet2))))))

(defun load-input(file)
  (setf *input* (with-open-file (stream file)
                  (loop for line = (read-line stream nil)
                        while line
                        collect (let* ((parts (split " " line))
                                       (cards (ts (car parts)))
                                       (bid (parse-integer (cadr parts))))
                                  (make-bet
                                   :hand (make-hand :cards cards :type (joker-calculate-hand-type cards))
                                   :bid bid)) into bets
                        finally (return (sort bets #'bet<))))))

;; part 1
(load-input "input")
(loop
  with count = (length *input*)
  for index = 1 then (1+ index)
  for bet in *input*
  summing (* index (bet-bid bet)))
