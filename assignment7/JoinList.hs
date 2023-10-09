{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}
module JoinList where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) list1 list2 = Append (tag list1 <> tag list2) list1 list2

getSizeTag :: (Monoid b, Sized b) => JoinList b a -> Int
getSizeTag = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ _ (Single _ innerVal) = Just innerVal
indexJ i joinList
  | i >= getSizeTag joinList = Nothing
indexJ index (Append currentTag lhs rhs)
  | index < (getSize (size currentTag) `div` 2) = indexJ index lhs
  | otherwise =
      let newIndex = index - 2
       in if newIndex >= 0
            then indexJ newIndex rhs
            else indexJ 0 rhs

testIndexJ :: Bool
testIndexJ = do
  let testInput = Append (Size 4) (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b")) (Append (Size 2) (Single (Size 1) "c") (Single (Size 1) "d"))
  and
    [ indexJ (-1) testInput == Nothing,
      indexJ 0 testInput == Just "a",
      indexJ 1 testInput == Just "b",
      indexJ 2 testInput == Just "c",
      indexJ 3 testInput == Just "d",
      indexJ 4 testInput == Nothing,
      indexJ 5 testInput == Nothing
    ]

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ index joinList -- Bounds checking
  | index <= 0 = joinList
  | index >= getSizeTag joinList = Empty
dropJ _ (Single _ _) = Empty -- Passed bounds check and drop anything on a single
dropJ index (Append cachedVal lhs rhs)
  | index == getSize (size cachedVal) = Empty
  | index <= (getSize (size cachedVal) `div` 2) = dropJ index lhs +++ rhs
  | otherwise = Empty +++ dropJ (index - 2) rhs

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ index joinList -- Bounds checking
  | index <= 0 = Empty
  | index >= getSizeTag joinList = joinList
takeJ _ (Single b a) = Single b a -- Passed bounds check
takeJ index (Append cachedVal lhs rhs)
  | index == getSize (size cachedVal) = lhs +++ rhs
  | index <= (getSize (size cachedVal) `div` 2) = takeJ index lhs
  | otherwise = lhs +++ takeJ (index - 2) rhs

main :: IO ()
main = do
  let testInput = Append (Size 4) (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b")) (Append (Size 2) (Single (Size 1) "c") (Single (Size 1) "d"))
  print $ takeJ 1 testInput
  print $ takeJ 2 testInput
  print $ takeJ 3 testInput
  print $ takeJ 4 testInput
