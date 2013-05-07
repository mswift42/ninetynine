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
