(defpackage #:day13 (:use :cl) (:export #:main))
(in-package :day13)

(defparameter *favorite* 1364)
(defparameter *max-distance* 50)

(defun open-p (p)
  (let ((x (realpart p))
        (y (imagpart p)))
    (zerop (mod (logcount (+ (* x x)
                             (* 3 x)
                             (* 2 x y)
                             y
                             (* y y)
                             *favorite*)) 2))))

(defun shortest-path (w h root goal)
  (let ((A (make-array (list (1+ w) (1+ h)) :initial-element sb-ext:double-float-positive-infinity))
        (count 0))
    (labels ((flood (p d)
               ;; have we reached a border?
               (symbol-macrolet ((Ap (aref A (realpart p) (imagpart p))))
                 (unless (and (< d *max-distance*)
                              (<= 0 (realpart p) w)
                              (<= 0 (imagpart p) h)
                              (< d Ap)
                              (open-p p))
                   (return-from flood)))
               (when (sb-ext:float-infinity-p Ap)
                 (incf count))
               (setf Ap (float d))
               ;; if any path exists, we know we can stop immediately if d exceeds it
               (when (and goal
                          (>= d (aref A (realpart goal) (imagpart goal))))
                 (return-from flood d))
               ;; fill each adjacency
               (loop for adj in '(-1 1 #c(0 1) #c(0 -1))
                     for v = (or (flood (+ p adj) (1+ d)) sb-ext:double-float-positive-infinity)
                     minimizing v)))
      (values (flood root 0) count))))

(defun visualize (A goal n)
  (loop for y from 0 below (array-dimension A 1)
        do (loop for x from 0 below (array-dimension A 0)
                 do (format t "~C" (cond
                                     ((and (= x (realpart goal)) (= y (imagpart goal))) #\*)
                                     ((not (open-p (complex x y))) #\#)
                                     ((sb-ext:float-infinity-p (aref A x y)) #\Space)
                                     ((< (- n (aref A x y)) 10) (digit-char (truncate (- n (aref A x y)))))
                                     (t #\.))))
        do (format t "~%")))

(defun main (argv)
  #+sbcl(sb-ext:disable-debugger)
  (let ((initial-p (member "--initial" argv :test #'equal)))
    (format *standard-output* "~A~%"
            (nth (if initial-p 0 1)
                 (multiple-value-list
                  (shortest-path (read) (read) #c(1 1)
                                 (when initial-p (complex (read) (read)))))))))
