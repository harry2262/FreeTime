import Data.Sequence (Seq (Empty))

-- Polymorphism
-- Haskell supports polymorphism for both data types and functions.

data List t = E | C t (List t) deriving (Show)

-- Integer List from Polymorphic datatype
list1 :: List Integer
list1 = C 5 (C 2 E)

-- Char List from Polymorphic datatype
list2 :: List Char
list2 = C 'J' (C 'A' (C 'S' E))

-- Polymorphic Functions

filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)
