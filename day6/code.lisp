;; d = tm - t^2
(ql:quickload :computable-reals)
(use-package :computable-reals)

(defparameter *times* '(51 69 98 78))
(defparameter *records* '(377 1171 1224 1505))

(defun distance (time max-time)
  (- (* time max-time) (expt time 2l0)))

(defun quadratic (a b c)
  (macrolet ((plus-or-minus-div (x y z)
               `(values (/ (+ ,x ,y) ,z) (/ (- ,x ,y) ,z))))
    (plus-or-minus-div (- b) (sqrt (- (* b b) (* 4 a c))) (* 2 a))))

(defun find-min-max (distance max-time)
  "find the min and max values that gets you further than distance"
  (multiple-value-bind (min max) (quadratic -1.0 max-time (* -1.0 distance))
    (values (+ 1.0 (floor min)) (- 1.0 (ceiling max)))))

(defun ways-to-win (record max-time)
  (multiple-value-bind (min max) (find-min-max record max-time)
    (1+ (- max min))))

;; part 1
(apply #'* (loop for time in *times*
                   for record in *records*
                   collecting (ways-to-win record time)))

;; part 2
(< (multiple-value-bind (min max) (quadratic -1l0 51699878l0 -377117112241505l0)
     (- (distance min 51699878l0) 377117112241505l0)
     ) 0.000001)


(distance 8788222 51699878)
(find-min-max 377117112241505.0 51699878.0)
(> (distance 8788223 51699878) 377117112241505)
(ways-to-win 377117112241505 51699878)




8788223
42911655
