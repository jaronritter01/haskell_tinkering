import Data.Char (digitToInt)

{-
Jaron Ritter
Assignment 1
23-8-2023
-}

{-
    Turns a number into a list of numbers.
    eg. 1234 -> [1, 2, 3, 4]
-}
toDigits :: Integer -> [Integer]
toDigits number
  | number <= 0 = []
  | otherwise = [toInteger (digitToInt char) | char <- show number]

{-
    Turns a number into a list of numbers but in reverse order
    eg. 1234 -> [4, 3, 2, 1]
-}
toDigitsRev :: Integer -> [Integer]
toDigitsRev number = reverse (toDigits number)

{-
    This will double every other number in a list
    eg. [1, 2, 3, 4] -> [1, 4, 3, 8]
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [element] = [element]
doubleEveryOther (first : second : remaining) = first : second * 2 : doubleEveryOther remaining

{-
    Given a digit, turn that digit into a list of inetegers and sum them.
    eg. 123 -> [1, 2, 3] -> 6
-}
sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

{-
    Given a list of integers, sum them. This accepts single and double digit entries.
    eg. 1, 12, 3, 14 -> [1, 1, 2, 3, 1, 4] -> 12
-}
sumDigits :: [Integer] -> Integer
sumDigits d = sum (map sumDigit d)

{-
    Used to validate credit card numbers
-}
validate :: Integer -> Bool
validate number = sumDigits (doubleEveryOther (toDigitsRev number)) `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 pegOne pegTwo pegThree = [(pegOne, pegThree)]
hanoi n pegOne pegTwo pegThree = (pegOne, pegThree) : hanoi (n - 1) pegOne pegThree pegTwo ++ hanoi (n - 1) pegThree pegOne pegTwo

main :: IO ()
main = do
  --   let number1 = 4012888888881881
  --   print (validate number1)
  --   let number2 = 4012888888881882
  --   print (validate number2)

  print (hanoi 2 "a" "b" "c")