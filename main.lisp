(in-package #:ngar)

(defun num-to-list (num dim)
  (labels ((get-bit (k)
             (logand 1 (ash num (- k)))))
    (reverse (loop for i from 0 to dim collect
                  (get-bit i)))))

(defun list-to-num (alist)
  (reduce #'+ (mapcar #'*
                      (reverse alist)
                      (mapcar (curry #'ash 1) 
                              (iota (length alist))))))

(defun get-all-masks-num (dim)
  (loop for i from 0 below (ash 1 dim) collect i))

(defun list-num-to-list (dim masks)
  (mapcar (rcurry #'num-to-list (1- dim))
          masks))

(defun get-all-masks (dim)
  (list-num-to-list dim
    (get-all-masks-num dim)))

(defun contains (l1 l2)
  (equal l2 (mapcar #'* l1 l2)))

(defun contains-num (x1 x2)
  (= l2 (logand x1 x2)))

(defun pretty-format (alist &key (os t))
  (format os "~{~a~%~}" alist)
  (format os "~a~%" (length alist)))

(defun apply-mask (alist mask)
  (if (= (length alist) (length mask))
    (labels ((lp (a m res)
               (if (null a) res
                   (let* ((a1 (cdr a))
                          (m1 (cdr m))
                          (chk (= (car m) 1))
                          (top (car a))
                          (res1 (if chk
                                    (cons top res)
                                    res)))
                     (lp a1 m1 res1)))))
      (lp alist mask nil))))

(defun set-of-all-subsets (alist)
  (let* ((masks (get-all-masks (length alist))))
    (loop for mask in masks collect
          (apply-mask alist mask))))

(pretty-format (set-of-all-subsets '(a b c)))

(pretty-format (set-of-all-subsets (get-all-masks 2)))

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

(defun gen-bulges (dim init-p)
  (let ((init-alist (get-all-masks-num dim)))
    ))

(pretty-format (get-hyperplane '(1 0 1)))


