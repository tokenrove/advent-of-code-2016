(defpackage #:day18 (:use :cl) (:export #:main))
(in-package :day18)

(defun trap-p (old)
  (destructuring-bind (l c r &rest _) old
    (declare (ignore c _))
    (logxor l r)))

(defun next-row (row)
  (loop for i below (length row)
        for old = (cons 0 (append row '(0))) then (cdr old)
        collecting (trap-p old)))

(defun visualize (initial count)
  (loop for i below count
        for row = initial then (next-row row)
        do (format t "~&~{~[.~;^~]~}" row)))

(defun count-safe (initial n)
  (loop for i below n
        for row = initial then (next-row row)
        sum (count 0 row)))

(defun from-string (s)
  (loop for c across s collect (if (char= c #\.) 0 1)))

(defun main (argv)
  #+sbcl(sb-ext:disable-debugger)
  (let ((initial-p (member "--initial" argv :test #'equal)))
    (format *standard-output* "~A~%"
            (count-safe (from-string (read-line))
                        (if initial-p 40 40000)))))
