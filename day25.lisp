(defpackage :day25 (:use :cl) (:export #:main))
(in-package :day25)

(defun read-insns (in)
  (coerce
   (let ((*package* (find-package :day25)))
     (loop for insn = (read in nil nil)
           while insn
           collecting
           `(,insn ,@(ecase insn
                       ((cpy jnz) (list (read in) (read in)))
                       ((inc dec out) (list (read in)))))))
   'vector))

(defun interp (insns &key (initial-a 0))
  (let ((pc 0))
    (dolist (s '(a b c d)) (setf (symbol-value s) 0))
    (setf (symbol-value 'a) initial-a)
    (loop while (< pc (array-dimension insns 0))
          for insn = (aref insns pc)
          with next = 0
          do (incf pc)
          do (destructuring-bind (op x &optional y) insn
               (when (and y (symbolp x)) (setf x (symbol-value x)))
               (ecase op
                 (inc (incf (symbol-value x)))
                 (dec (decf (symbol-value x)))
                 (out
                  (unless (= (symbol-value x) next) (return-from interp nil))
                  (setf next (logand 1 (lognot next)))
                  (format t "~&~A ~A" (symbol-value x) next))
                 (cpy (setf (symbol-value y) x))
                 (jnz (unless (zerop x) (incf pc (1- y)))))))
    (values (symbol-value 'a)
            (symbol-value 'b)
            (symbol-value 'c)
            (symbol-value 'd))))

(defun main (argv)
  #+sbcl(sb-ext:disable-debugger)
  (format *standard-output* "~A~%"
          (interp (read-insns *standard-input*)
                  :initial-a (read-from-string (second argv)))))
