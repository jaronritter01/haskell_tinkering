module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib $ iterate (+ 1) 0

-- Exercise 2
fib' :: Integer -> [Integer] -> [Integer]
fib' 0 list = list
fib' n (ele1 : ele2 : _) = ele1 : fib' (n - 1) [ele2, ele1 + ele2]

fibs2 :: [Integer]
fibs2 = fib' 5 [0, 1]

main :: IO ()
main = print fibs2