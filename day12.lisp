(defun read-insns (in)
  (coerce
   (loop for insn = (read in nil nil)
         while insn
         collecting
         `(,insn ,@(ecase insn
                     ((cpy jnz) (list (read in) (read in)))
                     ((inc dec) (list (read in))))))
   'vector))

(defun interp (insns &key (initial-c 0))
  (let ((pc 0))
    (dolist (s '(a b d)) (setf (symbol-value s) 0))
    (setf (symbol-value 'c) initial-c)
    (loop while (< pc (array-dimension insns 0))
          for insn = (aref insns pc)
          do (incf pc)
          do (destructuring-bind (op x &optional y) insn
               (when (and y (symbolp x)) (setf x (symbol-value x)))
               (ecase op
                 (inc (incf (symbol-value x)))
                 (dec (decf (symbol-value x)))
                 (cpy (setf (symbol-value y) x))
                 (jnz (unless (zerop x) (incf pc (1- y)))))))
    (symbol-value 'a)))

(defun main (argv)
  #+sbcl(sb-ext:disable-debugger)
  (format *standard-output* "~A~%"
          (interp (read-insns *standard-input*)
                  :initial-c (if (member "--initial" argv :test #'equal) 0 1))))
