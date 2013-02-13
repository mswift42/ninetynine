{- Ninety Nine Haskell Problems (derived from 99 Lisp & Prolog Problems) -}

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


