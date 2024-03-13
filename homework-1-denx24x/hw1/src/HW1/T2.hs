module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural
import Data.Maybe

data N = Z | S N deriving Show

nplus :: N -> N -> N
nplus Z b = b
nplus (S a) b = nplus a (S b)

nmult :: N -> N -> N
nmult Z b = Z
nmult (S Z) b = b
nmult (S a) b  = nplus b (nmult a b)

nsub :: N -> N -> Maybe N
nsub Z (S a) = Nothing
nsub a Z = Just a
nsub (S a) (S b) = nsub a b 

ncmp :: N -> N -> Ordering
ncmp a b = case nsub a b of 
              Nothing -> LT
              Just Z -> EQ
              otherwise -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural v = S (nFromNatural (v - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S v) = (nToNum v) + 1

nEven :: N -> Bool
nEven Z = True
nEven (S a) = nOdd a

nOdd :: N -> Bool
nOdd Z = False
nOdd (S a) = nEven a

ndiv :: N -> N -> N
ndiv _ Z = error "divide by zero"
ndiv a b | ncmp a b == LT = Z
ndiv a b = (S (ndiv (fromJust $ nsub a b) b))

nmod :: N -> N -> N
nmod _ Z = error "divide by zero"
nmod a b = fromJust $ nsub a $ nmult b $ ndiv a b
