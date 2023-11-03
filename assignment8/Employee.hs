module Employee where

import Data.Tree

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp {empName :: Name, empFun :: Fun}
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany =
  Node
    (Emp "Stan" 9)
    [ Node
        (Emp "Bob" 2)
        [ Node
            (Emp "Joe" 5)
            [ Node (Emp "John" 1) [],
              Node (Emp "Sue" 5) []
            ],
          Node (Emp "Fred" 3) []
        ],
      Node
        (Emp "Sarah" 17)
        [ Node (Emp "Sam" 4) []
        ]
    ]

testCompany2 :: Tree Employee
testCompany2 =
  Node
    (Emp "Stan" 9)
    [ Node
        (Emp "Bob" 3) -- (8, 8)
        [ Node
            (Emp "Joe" 5) -- (5, 6)
            [ Node (Emp "John" 1) [], -- (1, 0)
              Node (Emp "Sue" 5) [] -- (5, 0)
            ],
          Node (Emp "Fred" 3) [] -- (3, 0)
        ],
      Node
        (Emp "Sarah" 17) -- (17, 4)
        [ Node (Emp "Sam" 4) [] -- (4, 0)
        ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL currentList currentFun) = GL (employee : currentList) (empFun employee + currentFun)

instance Monoid GuestList where
  mempty = GL [] 0

instance Semigroup GuestList where
  GL list1 fun1 <> GL list2 fun2 = GL (list1 ++ list2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL list1 fun1) (GL list2 fun2)
  | fun1 >= fun2 = GL list1 fun1
  | otherwise = GL list2 fun2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold foldingFunc startingValue (Node value children) = foldingFunc value (map (treeFold foldingFunc startingValue) children)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where withoutBossL   = map fst bestLists
        withoutSubBoss = map snd bestLists
        withBossL      = map (glCons boss) withoutSubBoss

maximumS ::(Monoid a, Ord a) => [a] -> a
maximumS [] = mempty
maximumS lst = maximum lst

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max res
  where res = treeFold nextLevel (mempty, mempty) tree