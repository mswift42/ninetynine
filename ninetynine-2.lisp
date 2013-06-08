(load "~/ninetynine/ninetynine-1.lisp")
(load "~/quicklisp/setup.lisp")
(ql:quickload "lisp-unit")
;; pr21 Insert an element at a given position into a list.

(defun insert-at (element list n)
  (append (take (1- n) list) (list element) (drop (1- n) list)))

;; pr22 Create a list containing all integers within a given range
;; (range 4 9) -> (4 5 6 7 8 9)

(defun range (start end)
  (loop
       for i from start to end
       collecting i))

;; pr23 extract a number of randomly selected items from a list.

(defun rnd-select (list n)
  (loop
       for a = 0 then (+ a 1) and l = list then (remove-at l (random (length l)))
       when (= a n) return l))


;; pr24 lotto draw n different numbers from the set 1..M.

(defun lotto (n upto)
  (loop
     for a = nil then (cons (1+ (random upto)) a)
     when (= (length (remove-duplicates a)) n) return (values (remove-duplicates a))))


;; pr25 Generate a random permutation of the elements in a list.

;; (defun permute (list)
;;   (let* ((l (length list)) (r (random l)) (n (random l)))
;;     (insert-at (nth list r) (remove (nth list r) list) n)))


;; pr26 generate the combinations of k distinct objects chosen from the n elements of a
;; list. combination 3 '(a b c d ef) -> ((a b c) (a b d) (a b ef) ...)
(defun combinations (count list)
  (cond
    ((zerop count) '(())) ; one combination of zero element.
    ((endp list)   '())   ; no combination from noe element.
    (t (nconc (mapcar (let ((item (first list))) (lambda (combi) (cons item combi)))
                      (combinations (1- count) (rest list)))
              (combinations count (rest list))))))


;; pr27 lenght of combinations

(defun l-of-comb (count list)
  (length (combinations count list)))

;;pr28 Sort a list according to length of sublists
(defun smallerp (l1 l2)
  (< (length l1) (length l2)))

(defun sort-by-length (lst)
  (sort lst #'smallerp))

;; pr31 determine weather a given integer is prime.
(defun is-evenly-disible-p (a b)
  (zerop (mod a b)))

(defun is-prime-p (n)
  (case n
    (1 nil)
    (2 t)
    (otherwise (notany #'(lambda (x) (is-evenly-disible-p n x))
		       (loop for i from 2 to (sqrt n) collect i)))))


;; pr32 define greatest common divisors of two numbers.
(defun my-gcd (a b)
  (if (zerop b) a
      (my-gcd b (mod a b))))

(lisp-unit:define-test test-gcd
  (lisp-unit:assert-equal 2 (my-gcd 8 2))
  (lisp-unit:assert-equal (gcd 144 112) (my-gcd 144 112)))

;; pr33 define weather 2 pos. integers are coprimes (their gcd == 1)
(defun coprimep (a b)
  (= 1 (gcd a b)))

(lisp-unit:define-test test-coprimep
  (lisp-unit:assert-true (coprimep 35 64))
  (lisp-unit:assert-false (coprimep 144 112)))

;; pr34 calculate euler's totient function phi(m) (numbers of integers 1<=n<m that are coprime)

(defun totient (n)
  (loop for i from 1 below n
        count (coprimep n i)))

(lisp-unit:define-test test-totient
  (lisp-unit:assert-equal 4 (totient 10)))

;; pr35  Determine the prime factors of a given number

(defun prime-factors (n)
  (let ((primes (remove-if-not #'is-prime-p (loop for i from 2 to n collect i))))
    (remove-if-not #'(lambda (x) (is-evenly-disible-p n x)) primes)))




(lisp-unit:define-test test-prime-factors
  (lisp-unit:assert-equal '(3 5 7) (prime-factors 315)))


;; pr37 improved totien function phi(m)

(defun impr-totient (m)
  (labels ((rec (n count)
    (cond ((= n 1) (1+ count))
	  ((coprimep m n)
	   (rec (1- n) (1+ count)))
	  (t
	   (rec (1- n) count)))))
    (rec m 0)))

(lisp-unit:define-test test-impr-totient
  (lisp-unit:assert-equal 4 (impr-totient 10)))

;; pr39 list of all prime numbers in a given range
(defun primes (start stop)
  (remove-if-not #'is-prime-p (loop for i from start to stop collect i)))



(lisp-unit:run-tests)





