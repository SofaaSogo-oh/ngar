(in-package #:ngar)

(defun get-basis (vect)
  (let ((dim (length vect))
        (num (list-to-num vect)))
    (loop for i from 0 to dim unless 
             (zerop (logand (ash 1 i) num))
             collect (ash 1 i))))

(defun get-hyperplane (vect)
  (let* ((dim (1- (length vect)))
         (basis (get-basis vect))
         (combs (set-of-all-subsets basis))
         (lin-combs (mapcar (curry #'apply #'logior) combs)))
    (mapcar (rcurry #'num-to-list dim)
            lin-combs)))

(defun get-cover (num masks)
  (let* ((oasks (mapcar (curry #'logior num) masks))
         (axe (mapcar (lambda (x y) (and (= x y) x)) masks oasks)))
    (cdr (remove nil axe))))

(set-of-all-subsets (get-cover 5 '(1 2 4 5 8 7 9 10 13)))

(get-cover 0 '(0 1 2 3 4 5 6 7))

(defun gen-bulges (init-p cov &key 
                          (sub-cov #'get-cover))
  (let ((memo-table (make-hash-table :test #'equal)))
    (labels 
        ((is-memo (x) (gethash x memo-table))
         (memo (x) (setf (gethash x memo-table) t))
         (iter (q r)
           (if (null q) 
               r
               (let ((x (car q)))
                 (unless (is-memo x)
                   (memo x)
                   (let* ((p (funcall sub-cov x cov))
                          (q1 (append (cdr q) p))
                          (q2 (remove-if #'is-memo q1))
                          (r1 (append r p)))
                     (iter q2 r1)))))))
      (remove-duplicates (iter (list init-p) (list init-p))))))


(defun order-depends-combs (dim)
  (format t "~{~a~%~}"
    (loop for i from 0 to (1- (ash 1 dim)) collect
      (mapcar (rcurry #'num-to-list (1- dim))
              (gen-bulges i (get-all-masks-num dim))))))

(order-depends-combs 4)

(pretty-format (get-hyperplane '(1 0 1)))
