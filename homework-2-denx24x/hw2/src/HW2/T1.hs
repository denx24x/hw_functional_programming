module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ empty Leaf = empty
tfoldr f empty (Branch _ left v right) = tfoldr f (f v (tfoldr f empty right)) left 
