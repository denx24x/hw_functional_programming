module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.List ()

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = fromList [[]]
splitOn sep (x:xs) | x == sep = fromList [[]] <> splitOn sep xs
splitOn sep (x:xs) = let (first :| other) = splitOn sep xs in (x : first) :| other 

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = foldl (\a b -> a ++ (sep : b)) x xs
