-- James Bryant
-- COP 4020
-- Homework 2 Problem 1
module SimpleFunctions where

-- a) Removes first element in list that does NOT
--    satisfy the predicate function.
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst pred (x:xs)
  | pred x        = x:filterFirst pred xs
  | otherwise     = xs

-- b) Removes last element in list that does NOT
--    satisfy the predicate function.
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast pred xs = 
  reverse( filterFirst pred (reverse xs) )

-- c) Split the elements into tuple of 2 lists. Pick elements
--    alternately.
split :: [a] -> ([a],[a])
split [] = ([], [])   -- empty list; base case
split [x] = ([x], [])
split [x,y] = ([x], [y])
split xs = foldr (\x (odds, evens) -> (x:evens, odds) ) ([], []) xs

-- d) Interleave 2 list from a tuple
interleave :: ([a],[a]) -> [a]
interleave ([], [])   = []
interleave (x, [])  = x
interleave ([], x) = x
interleave (x:xs, y:ys) = [x, y] ++ interleave (xs, ys) 

-- e) Needs work
merge :: (Ord a) => ([a],[a]) -> [a]
merge ([], []) = []
merge (xs,[]) = xs
merge ([], ys) = ys
merge (x:xs, y:ys)
  | x <= y = x: merge (xs, y:ys)
  | otherwise = y:merge (x:xs, ys) 


-- f)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge( mergeSort as, mergeSort bs )
  where (as, bs) = split xs

