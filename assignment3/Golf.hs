module Golf where

{-
This function take in a list of elements and an integer.
The list is zipped with an enumeration the the same lenght as the list
    e.g: [1, 2, 4] -> (1, 0) (2, 1) (4, 2).
It is then filtered by the second value mod input integer equals 0 to get the elements whose index is a mutiple of the input integer.
    e.g.: (1, 0) (2, 1) (4, 2) -> 2 -> (4, 2)
Then those resulting pairs have their first value returned.
    e.g.: (4, 2) -> 4
-}
getNthElements :: [a] -> Integer -> [a]
getNthElements [] index = []
getNthElements inputList nthIndex = map fst (filter (\(value, index) -> ((index + 1) `mod` nthIndex == 0 && index /= 0) || nthIndex == 1) (zip inputList [0 ..]))

{-
This function takes in a list of elements.
The list is repeated length of list times and zipped with a list starting at 1 and is of the same length as the input list.
    e.g.: [7, 10, 3] -> [([7, 10, 3], 1), ([7, 10, 3], 2), ([7, 10, 3], 3)]
The getNthElement function is then mapped to each entry in the list with the first value of the pair being used as the first
input to the function and the second value used as the second input to the function.
    e.g.: [([7, 10, 3], 1), ([7, 10, 3], 2), ([7, 10, 3], 3)] -> [[7, 10, 3], [10], [3]]
-}
skips :: [a] -> [[a]]
skips [] = []
skips list = [uncurry getNthElements pair | pair <- zip [list | value <- [1 .. length list]] [1 ..]]

{-
This function take in a list and will check it 3 elements at a time to see if the second element is larger than the other two.
If so it will return a list of a single element. If not, it will return an list with no element.
This function will then remove an element from the beginning of the list and repeat.
-}

localMaxima :: [Integer] -> [Integer]
localMaxima (first : second : third : remaining) = [second | second > first && second > third] ++ localMaxima (second : third : remaining)
localMaxima _ = []

{-
Given a list of Intgers and value, count the number of occurrences of that value in the list.
-}
countOccs :: [Integer] -> Integer -> Integer
countOccs [] _ = 0
countOccs (num : nums) value =
  if num == value
    then 1 + countOccs nums value
    else 0 + countOccs nums value

{-
Given a list of Integers, get the numbers of occurences of all numbers 0 to 9 and place those counts in the respective indexs.
    e.g.: [1, 1, 1, 1, 5] -> [0, 4, 0, 0, 0, 1, 0, 0, 0, 0]
-}
getCountList :: [Integer] -> [Integer]
getCountList nums = zipWith countOccs ([nums | value <- [0 .. 9]]) [0 ..]

{-
Generate a line in the histogram given the count list and the vertical line to generate.
    e.g.: [0, 4, 0, 0, 0, 1, 0, 0, 0, 0] 0 -> " *   *    "
    e.g.: [0, 4, 0, 0, 0, 1, 0, 0, 0, 0] 1 -> " *        "
    e.g.: [0, 4, 0, 0, 0, 1, 0, 0, 0, 0] 7 -> "          "
-}
genLine :: [Integer] -> Integer -> String
genLine [] _ = "\n"
genLine (first : counts) num =
  if first > num
    then "*" ++ genLine counts num
    else " " ++ genLine counts num

{-
Create a histogram by
    1. Generating the count list
    2. Representing the historgram as line starting from the bottom at index 0
    3. Generate the correct string for each line 0 - 9
    4. Concatenate those lines with newline
    5. Concatenate that string with "==========\n" ++ "0123456789\n"
-}
histogram :: [Integer] -> String
histogram nums = do
  let countList = getCountList nums
  concat [genLine countList index | index <- reverse [0 .. 9]] ++ "==========\n" ++ "0123456789\n"

-- To Build ghc -main-is Golf -o main Golf.hs

main :: IO ()
main =
  print (histogram [1, 1, 1, 1, 5])