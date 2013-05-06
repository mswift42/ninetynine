;;;; 99 nine problems in Lisp 

;; pr01
(defun my-last (l)
  (cond
       ((null l) nil)
       ((null (cdr l)) (car l))
       (t (my-last (cdr l)))))


;; pr02 - find the last but one box of a list
;; Example: * (my-but-last '(a b c d)
;;          (C D)
(defun my-but-last (l)
  (cond
    ((null l) nil)
    ((null (cdr l)) nil)
    ((null (cddr l)) l)
    (t (my-but-last (cdr l)))))

;; pr03 find the n-th Element of a list
;; Example: * (element-at '(a b c d) 3)
;;          C
(defun element-at (l e)
  (cond
    ((null l) nil)
    ((= 1 e) (car l))
    (t (element-at (cdr l) (1- e)))))

;; pr04 find the length of a list
(defun my-length (l)
  (cond
    ((null l) 0)
    (t (1+ (my-length (cdr l))))))

;; pr05 reverse a list
(defun init (list)
  (cond
    ((null list) nil)
    ((null (cdr list)) nil)
    (t (cons (car list) (init (cdr list))))))

(defun my-reverse (list)
  (cond
    ((null list) nil)
    (t (cons (my-last list) (my-reverse (init list))))))

;; pr06 find out weather a list is a palindrome
(defun is-palindrome (list)
  (equal list (my-reverse list)))

;; pr07 Flatten a nested list.
(defun flatten (list)
  (cond
    ((null list) nil)
    ((atom (car list)) (cons (car list) (flatten (cdr list))))
    (t (append (flatten (car list)) (flatten (cdr list))))))

;; pr08 Eliminate consecutive duplicates in a list.
;; Example : compress '(a a a a b c c a a d e e e e)
;;           (a b c a d e)

(defun compress (list)
  (cond
    ((null list) nil)
    ((eq (car list) (cadr list)) (compress (cdr list)))
    (t (cons (car list) (compress (cdr list))))))

;; pr09 pack consecutive duplicates in a sublist.
(defun pack (list)
  (labels ((group-run (element group list)
             (cond
               ((null list) (list (cons element group)))
               ((eql element (first list)) (group-run element (cons element group) (rest list)))
               (t (cons (cons element group) (group-run (first list) '() (rest list)))))))
    (if (null list)
        '()
        (group-run (first list) '() (rest list)))))

;; pr10 Run length encoding of a list.
(defun run-length (list)
  (mapcar #'list (mapcar #'length (pack list))
	         (mapcar #'first  (pack list))))


;; pr11 Modified run-length encoding.
(defun modified-run (list)
  (mapcar #'(lambda (x) (if (= 1 (first x)) (second x) x)) (run-length list)))


;;pr12 Decode a run-length encoded list.

(defun decode (list)
  (flatten (mapcar #'duplicate list)))

  
(defun duplicate (list)
  (loop
       for i from 1 to (first list)
       collect (second list)))

;;pr13 

(defun modified-run (list)
  (mapcar #'(lambda (x) (if (= 1 (first x)) (second x) x)) (run-length list)))

;; pr14 duplicate the elements of a list
(defun dupli-2 (list)
  (loop
       for i from 1 to (* 2 (first list))
       collect (second list)))

(defun duplicate-pr14 (list)
  (flatten (mapcar #'dupli-2 (run-length list))))

;; pr15 replicate the elements of a list nth times.
(defun dupli-n (list n)
  (loop
       for i from 1 to (* n (first list))
       collect (second list)))

(defun replicate-n (list n)
  (flatten (mapcar #'(lambda (x) (dupli-n x n)) (run-length list))))

;; pr16 drop every nth item from a list.
(defun take (n list)
  (cond
    ((null list) nil)
    ((zerop n) nil)
    (t (cons (first list) (take (1- n) (rest list))))))

(defun drop (n list)
  (cond
    ((null list) nil)
    ((zerop n) list)
    (t (member (nth n list) list))))

(defun drop-n (list n)
  (cond
    ((null list) nil)
    (t (append (take (1- n) list)
	     (drop-n  (drop n list) n)))))

;; pr17 Split list in two parts . Length of first part is given.


