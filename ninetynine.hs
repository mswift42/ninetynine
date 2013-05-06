{- Ninety Nine Haskell Problems (derived from 99 Lisp & Prolog Problems) -}

import Data.List



-- Find the last elemend of a list
pr1 xs = last xs

-- find the last but one element of a list
pr2 [] = error "empty list"
pr2 xs = head $ drop (length xs -2) xs

-- Find the nth element of a list
pr3 [] _ = error "empty list"
pr3 xs n = xs !! (n-1)

-- find the length of a list
pr4 [] = 0
pr4 (x:xs) = 1 + pr4 xs

-- reverse a list
pr5 [] = []
pr5 xs = last xs : pr5 (init xs)

-- find out whether a list is a palindrome.
pr6 [] = False
pr6 xs = pr5 xs == xs


--flatten a nested List structure
pr7 [] = []
pr7 xs = head xs ++ pr7 (tail xs)


--Eliminate consecutive duplicates.
pr8 [] = []
pr8 (x:[]) = [x]
pr8 (x:y:xs) = if x == y then y : pr8 xs else [x] ++ pr8 ([y] ++ xs)

-- Pack consecutive duplicates into sublists.
pr9 xs = groupBy (==) xs


{- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. -}
pr10  xs = zip (map length (pr9 xs)) (map head (pr9 xs))


{- Modify the result of problem 10
that if an element has no duplicates it is
copied into the result list. -}
pr11 xs = zip (map multi (pr9 xs)) (map head (pr9 xs))
  where multi xs = if length xs > 1 then "multiple" else "single"


pr12 [] = []
pr12 (x:xs) = replicate (fst x) (snd x) ++ pr12 xs


-- List decoding directly
pr13 [] = []
pr13 xs = let groupedlist = group xs
          in if length (head groupedlist) > 1
             then (length (head groupedlist),head groupedlist):
                  pr13 (tail xs)
             else (1,head groupedlist) : pr13 (tail xs)


-- Duplicate the elements of a list.
pr14 [] = []
pr14 (x:xs) = replicate 2 x ++ pr14 xs


-- replicate elements of a list a given amount.
pr15 _ [] = []
pr15 n (x:xs) = replicate n x ++ pr15 n xs


-- drop every nth element of a list.
r16 _ [] = []

pr16 0 xs = xs
pr16 1 xs = []
pr16 n xs = take (n-1) xs ++ pr16 n (drop n xs)

-- split list into two parts. Length of first part is given.
pr17 _ [] = []
pr17 0 xs = [xs]
pr17 n xs = [take n xs] ++ [drop n xs]

-- extract a slice from a list.
pr18 xs s e = [xs !! i | i <- [s-1..e-1]]

pr19 [] _ = []
pr19 xs 0 = xs
pr19 xs n | n > 0 = drop n xs ++ take 3 xs
          | n < 0 = drop (length xs + n) xs ++ take (length xs + n) xs

-- remove the k-th element from a list.
pr20 1 (x:xs) = (x, xs)
pr20 n (x:xs) = (a, x:ys)
	where (a, ys) = pr20 (n - 1) xs



