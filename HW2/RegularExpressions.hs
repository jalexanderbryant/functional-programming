-- James Bryant
-- COP 4020
-- Homework 2 Problem 2
module RegularExpressions where

import Prelude hiding ((<*>))
import Data.Char

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char ->  RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
(|||) e1 e2 =
  \s -> e1 s || e2 s

splits :: [a] -> [([a],[a])]
-- splits "fun" ~~>
-- [("","fun"),("f","un"),
--  ("fu","n"),("fun","")]
splits xs =
  map (flip splitAt xs) [0..length xs]
-- alternatively, we could also use a list comprehension like so
-- [splitAt i xs | i <- [0..length xs]]

(<*>) :: RegExp -> RegExp -> RegExp
(<*>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<**>) :: RegExp -> RegExp -> RegExp
(<**>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon ||| (e <**> star e)

-- put your solutions here

-- option

option :: RegExp -> RegExp
option pattern = epsilon ||| pattern
-- plus

plus :: RegExp -> RegExp
plus pattern = pattern ||| (star pattern <**> pattern)

-- number

-- Collaborated with Linnette Martinez to produce pattern matching 
-- for 'number' function

number :: RegExp
number pattern
  | null pattern == True = False
  | pattern == "0" = True
  | isDigit(head pattern) == False = False
  | (head pattern) == '0' = False
  | otherwise = if (False `elem` str) then False else True
  where str = check_rest (tail pattern)
  

check_rest :: String -> [Bool]
check_rest "" = []
check_rest (x:xs) = if not (isDigit x) then [False] else check_rest xs ++ [True]

-- fractional number

fractional :: RegExp
fractional pattern
    | pattern == "0.0" = True
    | ((head pattern) == '0') && head(drop 1 pattern) == '.' = number (tail digits)
    | (last pattern) == '0' && (head (drop 1 ( reverse pattern) )) == '.' = number digits
    | (isDigit( head pattern ) == True) && (last pattern) /= '0' = number digits
    | otherwise = False
    where digits = ([x | x <- pattern, x /= '.' ])

    -- "Aaaaaaahhhhh.....I'm touching myself tonight" - DeadPool