module Main where

import Data.List

-- Exercise One
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum
    . filter even
    . takeWhile (/= 1)
    . iterate
      ( \x ->
          if even x
            then x `div` 2
            else x * 3 + 1
      )

-- Exercise 2 (Skipping for now)
-- data Tree a
--   = Leaf
--   | Node Integer (Tree a) a (Tree a)
--   deriving (Show, Eq)

-- Need this to insert a node correctly
-- insertNode :: Tree typ -> Tree typ
-- insertNode tree = Leaf

-- foldTree :: [a] -> Tree a
-- foldTree a = Leaf

-- Exercise 3-1
xor :: [Bool] -> Bool
xor = odd . foldr ((+) . fromEnum) 0

-- Exercise 3-2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- Exercise 4
seiveSundaram :: Integer -> [Integer]
seiveSundaram n = filter odd [1 .. (2 * n + 2)] \\ sort (concat [[i + j + (2 * i * j) | i <- [1 .. j]] | j <- [1 .. n]])

main :: IO ()
main = print $ seiveSundaram 5