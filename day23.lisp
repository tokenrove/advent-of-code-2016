(defpackage :day23 (:use :cl))
(in-package :day23)

(defun read-insns (in)
  (coerce
   (loop for insn = (read in nil nil)
         while insn
         collecting
         `(,insn ,@(ecase insn
                     ((cpy jnz) (list (read in) (read in)))
                     ((inc dec tgl) (list (read in))))))
   'vector))

(defun toggle (insn)
  (ecase insn
    (inc 'dec)
    ((dec tgl) 'inc)
    (jnz 'cpy)
    (cpy 'jnz)))

(define-modify-macro togglef () toggle)

(defun interp (insns &key (initial-a 0))
  (let ((pc 0))
    (dolist (s '(a b c d)) (setf (symbol-value s) 0))
    (setf (symbol-value 'a) initial-a)
    (loop while (< pc (array-dimension insns 0))
          for insn = (aref insns pc)
          do (incf pc)
          do (destructuring-bind (op x &optional y) insn
               (when (and y (symbolp x)) (setf x (symbol-value x)))
               (ecase op
                 (inc (incf (symbol-value x)))
                 (dec (decf (symbol-value x)))
                 (tgl (let ((where (+ pc (symbol-value x) -1)))
                        (when (<= 0 where (1- (array-dimension insns 0)))
                          (togglef (first (aref insns where))))))
                 (cpy (when (symbolp y) (setf (symbol-value y) x)))
                 (jnz (when (symbolp y) (setf y (symbol-value y))) (unless (zerop x) (incf pc (1- y)))))))
    (symbol-value 'a)))

(defun main (argv)
  #+sbcl(sb-ext:disable-debugger)
  (format *standard-output* "~A~%"
          (interp (read-insns *standard-input*)
                  :initial-a 7)))
