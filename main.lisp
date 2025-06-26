(in-package #:ngar)

(defun find-hull (x convex)
  (reduce #'get-intersection 
          (remove-if-not 
            (lambda (y) (contains y x))
            convex)))

(find-hull '(0 0 1) '((1 1 1) (0 1 1) (0 1 0)))

(defun get-all-masks-for-convex (convex)
  (let* ((dim (length (first convex)))
         (masks (get-all-masks dim)))
    masks))

(defun find-check-masks (masks ngar)
  (remove-if-not 
    (lambda (x) (<= (mask-norm x) ngar))
    masks))

(defun hulls-to-hashmap (convex check-masks)
  (let ((res-table (make-hash-table)))
    (loop for mask in check-masks do
          (setf 
            (gethash mask res-table) 
            (find-hull mask convex)))
    res-table))

(defun check-ngar-display (convex ngar applier)
  (let* ((masks (get-all-masks-for-convex convex))
         (check-masks (find-check-masks masks ngar))
         (g-table (hulls-to-hashmap convex check-masks)))
    (loop for mask in masks do
          (let ((d-mask (print-mask applier mask)))
            (format t "~a ~:[∉~;∈~] G:~%" 
                               mask
                               (member mask convex :test #'equal))
            (maphash (lambda (key value)
                       (let ((d-key (print-mask applier key))
                             (d-value (print-mask applier value)))
                         (if (contains mask key)
                           (format t "~a ⊂ ~a ⇒ g~a = ~a ~:[⊄~;⊂~] ~a~%"
                                   d-key d-mask d-key d-value (contains mask value) d-mask)
                           (format t "~a ⊄ ~a~%" d-key d-mask))))
                     g-table)))))


(check-ngar-display '((1 1 1) (1 1 0) (1 0 0)) 1 '(a b c))

(let ((table (find-hulls '((1 1 1 1) (0 1 1 1) (0 1 0 1)) 1)))
  (maphash 
   (lambda (key value) (format t "g: ~a → ~a~%" key value))
   table)
  (hash-table-count table))

(get-all-masks 3)

;(remove-if-not (lambda (x) (<= (length x) 1)) (set-of-all-subsets '(a b c)))

