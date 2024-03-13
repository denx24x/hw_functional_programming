module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1
import GHC.Real (Ratio(..))

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S a) = S (mapAnnotated f . a)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S (\s -> let ((S f2) :# res_s) = f s
                               (a2 :# res_s2) = f2 res_s in (a2 :# res_s2))

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState

  (<*>) (S f) (S g) = S (\s -> let (a :# s1) = g s
                                   (b :# s2) = f s1 in b a :# s2)

instance Monad (State s) where
  (>>=) (S f) g = S (\s -> let (a :# s1) = f s
                               (S f2) = g a
                               (b :# s2) = f2 s1 in b :# s2)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) a b = Op (Add a b)
  (-) a b = Op (Sub a b)
  (*) a b = Op (Mul a b)
  abs v = Op (Abs v)
  signum v = Op (Sgn v)
  fromInteger v = Val (fromInteger v)

instance Fractional Expr where
  (/) a b = Op (Div a b)
  fromRational (a :% b) = Op (Div (fromInteger a) (fromInteger b))

unaryProto :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> State [Prim Double] Double
unaryProto constr conv expr = do
  val <- eval expr
  modifyState (\s -> constr val : s)
  return (conv val)

binaryProto :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> State [Prim Double] Double
binaryProto constr conv a b = do
  left <- eval a 
  right <- eval b
  modifyState (\s -> constr left right : s)
  return (conv left right)


eval :: Expr -> State [Prim Double] Double
eval (Val v) = pure v

eval (Op (Abs expr)) = unaryProto Abs abs expr

eval (Op (Sgn expr)) = unaryProto Sgn signum expr 

eval (Op (Add a b)) = binaryProto Add (+) a b

eval (Op (Sub a b)) = binaryProto Sub (-) a b

eval (Op (Mul a b)) = binaryProto Mul (*) a b

eval (Op (Div a b)) = binaryProto Div (/) a b


