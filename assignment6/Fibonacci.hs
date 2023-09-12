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
fib' 0 list = init list
fib' n (ele1 : ele2 : _) = ele1 : fib' (n - 1) [ele2, ele1 + ele2]

fibs2 :: [Integer]
fibs2 = fib' 1 [0, 1]

-- Exercise 3
data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream first rest) = first : streamToList rest

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Stream first rest) = Stream (func first) (streamMap func rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed func x = Stream x (streamFromSeed func (func x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer

main :: IO ()
main = print nats