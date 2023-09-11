module Main where

import Data.List (sort, (\\))

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

-- I took this from https://github.com/OctaviPascual/cis194-IntroductionToHaskell/blob/master/homework-04/HigherOrder.hs becuase I was impressed with the solution
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Generate a balanced binary tree from a list of values
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Return the height of a tree
-- Note that since the height of a tree with a single node is defined as 0, we
-- define the height of a Leaf as -1 to be able to distinguish those two cases
height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

-- Insert a new node into an existing balanced binary tree
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left root right)
  | h1 < h2 = Node h (insert x left) root right
  | h1 > h2 = Node h left root (insert x right)
  | otherwise = Node (h3 + 1) left' root right
  where
    h1 = height left
    h2 = height right
    h3 = height left'
    left' = insert x left

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