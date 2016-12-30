
;;; read the input, saving into an array, and making note of the
;;; coordinates of numbered squares

;;; for each point, BFS to discover shortest paths to each other point

;;; for each permutation of points, find shortest path

(defun read-input (in)
  (let* ((lines (loop for line = (read-line in nil nil) while line collect line))
         (height (length lines))
         (width (length (first lines)))
         (walkable (make-array (list (1+ width) (1+ height))
                               :element-type 'bit
                               :initial-element 0))
         (markers (make-array 10 :initial-element nil)))
    (loop for y below height
          do (loop for x below width
                   for c = (aref (nth y lines) x)
                   do (setf (aref walkable x y)
                            (ecase c
                              (#\# 0)
                              (#\. 1)
                              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                               (setf (aref markers (digit-char-p c)) (complex x y))
                               1)))))
    (values walkable markers)))

(defun shortest-path (walkable root goal)
  (let* ((w (1- (array-dimension walkable 0)))
         (h (1- (array-dimension walkable 1)))
         (A (make-array (list (1+ w) (1+ h)) :initial-element sb-ext:double-float-positive-infinity)))
    (labels ((flood (p d)
               ;; have we reached a border?
               (symbol-macrolet ((Ap (aref A (realpart p) (imagpart p))))
                 (unless (and (<= 0 (realpart p) w)
                              (<= 0 (imagpart p) h)
                              (< d Ap)
                              (plusp (aref walkable (realpart p) (imagpart p))))
                   (return-from flood))
                 (setf Ap (float d)))
               ;; if any path exists, we know we can stop immediately if d exceeds it
               (when (and goal
                          (>= d (aref A (realpart goal) (imagpart goal))))
                 (return-from flood d))
               ;; fill each adjacency
               (loop for adj in '(-1 1 #c(0 1) #c(0 -1))
                     for v = (or (flood (+ p adj) (1+ d)) sb-ext:double-float-positive-infinity)
                     minimizing v)))
      (prog1 (flood root 0)
        #+nil(visualize walkable A goal)))))

(defun visualize (walkable A goal)
  (loop for y from 0 below (array-dimension A 1)
        do (loop for x from 0 below (array-dimension A 0)
                 do (format t "~C" (cond
                                     ((and (= x (realpart goal)) (= y (imagpart goal))) #\*)
                                     ((zerop (aref walkable x y)) #\#)
                                     ((sb-ext:float-infinity-p (aref A x y)) #\Space)
                                     ((< (aref A x y) 10) (digit-char (truncate (aref A x y))))
                                     (t #\.))))
        do (format t "~%")))

;;; could do this lazily as we compute tours, aborting them as they
;;; cost too much, but this doesn't take so long.
(defun compute-pair-costs (walkable markers n-markers)
  (let ((result (make-array (list n-markers
                                  n-markers)
                            :initial-element nil)))
    (loop for i below n-markers
          for source = (aref markers i)
          when source
            do (loop for j below n-markers
                     for sink = (aref markers j)
                     when sink
                       unless (or (eql source sink) (aref result i j))
                         ;; this would be much faster, also, if we did only one BFS for one sink, until we hit each marker
                         do (let ((cost (shortest-path walkable source sink)))
                              (setf (aref result i j) cost
                                    (aref result j i) cost))))
    result))

(defun cost-of-tour (pair-costs tour)
  (+ (loop for source = 0 then sink
           for sink across tour
           sum (aref pair-costs source sink))
     (aref pair-costs (aref tour (1- (length tour))) 0)))

(defun shortest-tour (n-markers pair-costs)
  (let ((tour (make-array n-markers))
        min-tour
        min-cost)
    (loop for i below n-markers do (setf (aref tour i) (1+ i)))
    (labels ((generate (n)
               (if (= n 1)
                   (let ((cost (cost-of-tour pair-costs tour)))
                     (unless min-cost (setf min-cost cost))
                     (when (< cost min-cost)
                       (setf min-cost cost
                             min-tour (copy-seq tour))))
                   (progn
                     (loop for i below (1- n)
                           do (progn
                                (generate (1- n))
                                (rotatef (aref tour (if (evenp n) i 0)) (aref tour (1- n)))))
                     (generate (1- n))))))
      (generate n-markers)
      (values min-cost min-tour))))
