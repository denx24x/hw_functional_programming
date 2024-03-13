module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = Meta Int Int deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (Meta size _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (Meta _ depth) _ _ _) = depth

recalc_tmeta :: Tree a -> Tree a
recalc_tmeta Leaf = Leaf
recalc_tmeta (Branch (Meta size depth) a v b) = (Branch 
                                                  (Meta 
                                                    ((tsize a) + (tsize b) + 1)
                                                    ((max (tdepth a) (tdepth b)) + 1)) 
                                                    a v b)

tmember :: Ord a => a -> Tree a -> Bool
tmember val Leaf = False
tmember val (Branch _ a v b) = case compare val v of 
                                  GT -> tmember val b
                                  EQ -> True
                                  LT -> tmember val a

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert val Leaf = (Branch (Meta 1 1) Leaf val Leaf)
tinsert val tree@(Branch meta a v b) = case compare val v of 
                                        GT -> balance $ recalc_tmeta $ (Branch meta a v (recalc_tmeta $ tinsert val b))
                                        EQ -> tree
                                        LT -> balance $ recalc_tmeta $ (Branch meta (recalc_tmeta $ tinsert val a) v b) 

diff :: Tree a -> Int
diff Leaf = 0
diff (Branch _ a _ b) = (tdepth a) - (tdepth b)

balance :: Tree a -> Tree a
balance tree@(Branch meta a v b) = case diff tree of
                                            -2 -> case diff b of
                                                        1 -> rotate_left_big tree 
                                                        otherwise -> rotate_left_small tree
                                            2 -> case diff a of
                                                        -1 -> rotate_right_big tree 
                                                        otherwise -> rotate_right_small tree
                                            otherwise -> tree 

rotate_left_small :: Tree a -> Tree a
rotate_left_small (Branch meta1 p a (Branch meta2 q b r)) = (recalc_tmeta $ Branch meta2 (recalc_tmeta $ Branch meta1 p a q) b r)

rotate_right_small :: Tree a -> Tree a
rotate_right_small (Branch meta1 (Branch meta2 r b q) a p) = (recalc_tmeta $ Branch meta2 r b (recalc_tmeta $ Branch meta1 q a p))

rotate_left_big :: Tree a -> Tree a
rotate_left_big (Branch meta1 a v b) = rotate_left_small (recalc_tmeta $ Branch meta1 a v (rotate_right_small b))

rotate_right_big :: Tree a -> Tree a
rotate_right_big (Branch meta1 a v b) = rotate_right_small (recalc_tmeta $ Branch meta1 (rotate_left_small a) v b)

tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (x : xs) = tinsert x $ tFromList xs 
