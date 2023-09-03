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
getNthElements inputList nthIndex = map fst (filter (\(value, index) -> ((index + 1) `mod` nthIndex == 0 && index /= 0 ) || nthIndex == 1) (zip inputList [0..]))

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
skips list = [uncurry getNthElements pair | pair <- zip [list | value <- [1..length list]] [1..]]

{-
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
-}



-- To Build ghc -main-is Golf -o main Golf.hs

main :: IO ()
main = print (skips "hello!")