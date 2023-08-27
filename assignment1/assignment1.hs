import Data.Char (digitToInt)

{-
Jaron Ritter
Assignment 1
23-8-2023
-}

toDigits :: Integer -> [Integer]
toDigits number
  | number <= 0 = []
  | otherwise = [toInteger (digitToInt char) | char <- show number]

toDigitsRev :: Integer -> [Integer]
toDigitsRev number = reverse (toDigits number)

-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther [] = []
-- doubleEveryOther [element] = [element]
-- doubleEveryOther (first : second : remaining)
--   | odd (length (first : second : remaining)) = first : (second * 2) : doubleEveryOther remaining
--   | even (length (first : second : remaining)) = (first * 2) : second : doubleEveryOther remaining

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [element] = [element]
doubleEveryOther (first : second : remaining) = first : second * 2 : doubleEveryOther remaining

sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits d = sum (map sumDigit d)

validate :: [Integer] -> Bool
validate list = sumDigits (doubleEveryOther list) `mod` 10 == 0

main :: IO ()
main = do
  let number = [16, 7]
  print (sumDigits number)