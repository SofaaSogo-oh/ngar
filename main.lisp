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

(defun check-ngar-display (masks convex check-masks applier &key (os t))
  (let* ((g-table (hulls-to-hashmap convex check-masks)))
    (format os "~&G = ~a ~%"
            (print-set (mapcar (curry #'print-mask applier) convex)))
    (loop for mask in masks do
          (let ((d-mask (print-mask applier mask)))
            (format os "~%~a ~:[∉~;∈~] G:~%" 
                               d-mask
                               (member mask convex :test #'equal))
            (maphash (lambda (key value)
                       (let ((d-key (print-mask applier key))
                             (d-value (print-mask applier value)))
                         (if (contains mask key)
                           (format os "~a ⊂ ~a ⇒ g~a = ~a ~:[⊄~;⊂~] ~a~%"
                                   d-key d-mask d-key d-value (contains mask value) d-mask)
                           (format os "~a ⊄ ~a~%" d-key d-mask))))
                     g-table)))))


(defun check-ngar (masks convex check-masks)
  (let ((g-table (hulls-to-hashmap convex check-masks)))
    (labels
        ((process-existance (mask)
           (maphash 
             (lambda (key value)
               (when
                   (and
                     (contains mask key)
                     (not (contains mask value)))
                 (return-from process-existance nil)))
             g-table)
           t)
         (process-mask (mask)
           (if (member mask convex :test #'equal)
               (process-existance mask)
               (not (process-existance mask)))))
      (mapcar #'process-mask masks))))

(set-of-all-subsets (get-all-masks 3))

(let* ((dim 3)
       (masks (get-all-masks dim))
       (sset (set-of-all-subsets masks))
       (cset (remove-if-not 
              (lambda (x) (and
                           (member (make-list dim :INITIAL-ELEMENT 1) x :test #'equal)
                           (member (make-list dim :INITIAL-ELEMENT 0) x :test #'equal)))
              sset))
       (chkm (find-check-masks masks 0)))
  (loop for convex in cset do
        (check-ngar-display masks convex chkm '(a b c))))

(defparameter *alphabet*
  '(a b c d e f g h k l m n o p r s q u w x y z))

(let* ((dim 2)
       (ngar 0)
       (masks (get-all-masks dim))
       (sset (set-of-all-subsets masks))
       (cset (remove-if-not 
              (lambda (x) (and
                           (member (make-list dim :INITIAL-ELEMENT 1) x :test #'equal)))
              sset))
       (chkm (find-check-masks masks ngar)))
  (loop for convex in cset do
        (let* ((pre-res (check-ngar masks convex chkm))
               (res (eval (cons 'and pre-res))))
          (format t "~a ↦ ~a ↦ ~a~%" (print-set (mapcar (curry #'print-mask (subseq *ALPHABET* 0 dim)) convex)) pre-res res))))

(let* ((dim 4)
       (ngar 4)
       (masks (get-all-masks dim))
       (sset (set-of-all-subsets masks))
       (cset (remove-if-not 
              (lambda (x) (and
                           (member (make-list dim :INITIAL-ELEMENT 1) x :test #'equal)))
              sset))
       (chkm (find-check-masks masks ngar)))
  (reduce #'+ 
          (loop for convex in cset collect
               (let* ((pre-res (check-ngar masks convex chkm))
                      (res (eval (cons 'and pre-res))))
                 (if res 1 0)))))

(get-all-masks 3)

;(remove-if-not (lambda (x) (<= (length x) 1)) (set-of-all-subsets '(a b c)))

(eval (cons 'and '(t nil t)))
(subseq '(a b c d e f) 0 3)
