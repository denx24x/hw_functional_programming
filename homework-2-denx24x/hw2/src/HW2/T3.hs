module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable ()

accumulator :: Monoid a => Maybe a -> a -> a
accumulator Nothing b = b
accumulator (Just v) b = v <> b

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr accumulator mempty

applyfunc :: (Monoid a, Monoid b) => Either a b -> (a, b)
applyfunc (Left a) = (a, mempty)
applyfunc (Right b) = (mempty, b)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap applyfunc 
