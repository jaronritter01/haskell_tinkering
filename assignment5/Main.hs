{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import ExprT
import Parser
import StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit val) = val
eval (ExprT.Add exp1 exp2) = eval exp1 + eval exp2
eval (ExprT.Mul exp1 exp2) = eval exp1 * eval exp2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr expr =
  let parsedExp = parseExp Lit ExprT.Add ExprT.Mul expr
   in case parsedExp of
        Just expression -> Just (eval expression)
        Nothing -> Nothing

{-
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)
-}

reify :: Mod7 -> Mod7
reify = id

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit num = Mod7 (num `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Either String StackVal
run = execute . compile
  where
    execute Nothing = Left "The program does not compile."
    execute (Just p) = stackVM p

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

main :: IO ()
main = print $ run "(3 * -4) + 5"