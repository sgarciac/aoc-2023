;; d = tm - t^2
(ql:quickload :computable-reals)
(use-package :computable-reals)
(setq *PRINT-PREC* 50)

(defparameter *times* '(51 69 98 78))
(defparameter *records* '(377 1171 1224 1505))

(defun distance (time max-time)
  (-r (*r time max-time) (expt-r time 2)))

(defun quadratic (a b c)
  (macrolet ((plus-or-minus-div (x y z)
               `(values (/r (+r ,x ,y) ,z) (/r (-r ,x ,y) ,z))))
    (plus-or-minus-div (-r b) (sqrt-r (-r (*r b b) (*r 4 a c))) (*r 2 a))))

(defun find-min-max (distance max-time)
  "find the min and max values that gets you further than distance"
  (multiple-value-bind (min max) (quadratic -1 max-time (*r -1 distance))
    (values (+r 1 (floor-r min)) (-r (ceiling-r max) 1))))

(defun ways-to-win (record max-time)
  (multiple-value-bind (min max) (find-min-max record max-time)
    (+r 1 (-r max min))))

;; part 1
(apply #'* (loop for time in *times*
                   for record in *records*
                   collecting (ways-to-win record time)))

;; part 2
(time (ways-to-win 377117112241505 51699878))
