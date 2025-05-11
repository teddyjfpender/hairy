module Tutorial.Recursion where

import Common.PreludeExtras

-- Some examples of recursive functions

-- Compute the length of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Fibonacci sequence
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2) 