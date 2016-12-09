;;; Simple Common Lisp implementation using complex numbers and
;;; treating L and R as multiplication by i and -i respectively.
;;;
;;; We do the second half of the behavior by naively checking for line
;;; intersections.

(defpackage #:day1 (:use :cl) (:export #:main))
(in-package :day1)

(defparameter *initial-behavior-p* nil)

(defun read-input (in)
  (flet ((1ch () (read-char in nil nil)))
    (loop for c = (1ch)
          while c
          collect (ecase c (#\R #c(0 -1)) (#\L #c(0 1))) into rotations
          collect (prog1 (read in) (1ch) (1ch)) into magnitudes
          finally (return (values rotations magnitudes)))))

(defun · (a b)
  (+ (* (realpart a) (realpart b))
     (* (imagpart a) (imagpart b))))

(defun ∘ (a b)
  (complex (* (realpart a) (realpart b))
           (* (imagpart a) (imagpart b))))

(defun on-line-p (a b p)
  (= (abs (- a b))
     (+ (abs (- a p)) (abs (- b p)))))

(defun point-of-intersection (a b c d)
  (let* ((ab-> (signum (- b a)))
         (cd-> (signum (- d c)))
         (p (if (zerop (· ab-> cd->))
                ;; orthogonal
                (+ (∘ ab-> c) (∘ cd-> a))
                ;; parallel
                (let* ((p (second (sort (list a b c d) #'<
                                        :key (lambda (x) (· ab-> x))))))
                  (+ (∘ ab-> p)
                     (∘ a (* #c(0 1) ab->)))))))
    (and (on-line-p a b p) (on-line-p c d p) p)))

(defun first-crossing-of (points)
  (loop with n = (1- (fill-pointer points))
        with a = (aref points (1- n)) and b = (aref points n)
        for i from 0 below (- n 2)
        when (point-of-intersection a b (aref points i) (aref points (1+ i)))
          return it))

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (when it ,@body)))

(defun compute (rotations magnitudes)
  (assert (= (length rotations) (length magnitudes)))
  (let ((points (make-array (1+ (length rotations))
                            :fill-pointer 1
                            :initial-element #c(0 0))))
    (loop for θ in rotations
          for m in magnitudes
          with heading = #c(0 1) and position = 0
          do (progn
               (incf position (* m (setf heading (* heading θ))))
               (vector-push position points)
               (awhen (and (not *initial-behavior-p*)
                           (first-crossing-of points))
                 (return-from compute it)))
          finally (return position))))

(defun manhattan-distance (c)
  (truncate (+ (abs (realpart c)) (abs (imagpart c)))))

(defun main (argv)
  #+sbcl(sb-ext:disable-debugger)
  (let ((*initial-behavior-p* (member "--initial" argv :test #'equal)))
    (format *standard-output* "~A~%"
            (manhattan-distance (multiple-value-call #'compute
                                  (read-input *standard-input*))))))
