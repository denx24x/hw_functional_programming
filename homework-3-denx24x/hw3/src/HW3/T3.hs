module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1
import HW3.T2 ( join )

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some a) = a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success a) = a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# (e1 <> e2)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (a :. other) = join a (joinList other)

getFun :: Fun i a -> (i -> a)
getFun (F g) = g

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> getFun (f i) i )
