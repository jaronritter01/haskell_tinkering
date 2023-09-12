module Fibonacci where

import Data.Bits

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
nats = streamFromSeed (+ 1) 0

helper :: Integral t => t -> [t]
helper 0 = []
helper n = let (q, r) = n `divMod` 2 in r : helper q

toBin :: Integral a => a -> [a]
toBin 0 = [0]
toBin n = reverse (helper n)

padBitToLength :: Integer -> [Integer] -> [Integer]
padBitToLength len bit =
  if toInteger (length bit) == len
    then bit
    else padBitToLength len (0 : bit)

binXor :: [Integer] -> [Integer] -> [Integer]
binXor bit1 bit2 =
  if length bit1 > length bit2
    then zipWith xor bit1 (padBitToLength (toInteger (length bit1)) bit2)
    else zipWith xor bit2 (padBitToLength (toInteger (length bit2)) bit1)

xorSum :: Integer -> Integer -> Integer
xorSum p1 p2 = sum $ binXor (toBin p1) (toBin p2)

xorSumStream :: Stream Integer -> Stream Integer -> Stream Integer
xorSumStream (Stream a resta) (Stream b restb) = Stream (sum $ binXor (toBin a) (toBin b)) (xorSumStream resta restb)

ruler :: Stream Integer
ruler = streamMap (subtract 1) $ xorSumStream (streamFromSeed (+ 1) 0) (streamFromSeed (+ 1) 1)

main :: IO ()
main = print $ ruler