(defpackage #:day16 (:use :cl))
(in-package :day16)

;;; could use plain bignums here, but we'd have to be fancier with
;;; reverse.

(defun dragon (a)
  (let ((b (bit-not (reverse a))))
    (concatenate 'bit-vector a #*0 b)))

(defun dragon-until (a n)
  (loop for v = a then (dragon v)
        while (< (length v) n)
        finally (return v)))

;;; It would be much cooler to xor the number against itself shifted
;;; right once, then extract the odd bits as if this were Morton
;;; encoded.
(defun checksum-once (v)
  #+nil(unmorton (logxor v (ash v 1)))
  (let* ((n (floor (length v) 2))
         (checksum (make-array n :element-type 'bit)))
    (loop for i from 0 below n
          do (setf (bit checksum i)
                   (logand 1
                           (logeqv (bit v (* 2 i))
                            (bit v (1+ (* 2 i)))))))
    checksum))

(defun checksum (a)
  (loop for v = (checksum-once a) then (checksum-once v)
        while (evenp (length v))
        finally (return v)))

(defun fill-up (start n)
  (checksum (subseq (dragon-until start n) 0 n)))
