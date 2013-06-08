(load "~/ninetynine/ninetynine-1.lisp")
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



