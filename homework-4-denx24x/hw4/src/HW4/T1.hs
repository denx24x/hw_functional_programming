module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES $ \s -> case g s of 
                                        (Error e) -> Error e
                                        (Success (a :# s2)) -> Success $ f a :# s2

wrapExceptState :: a -> ExceptState e s a
wrapExceptState val = ES $ \s -> Success (val :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \s -> case f s of 
                                          (Error e) -> Error e
                                          (Success ((ES g) :# s2)) -> case g s2 of 
                                              (Error e2) -> Error e2
                                              (Success (a :# s3)) -> Success (a :# s3)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) (ES f) (ES g) = ES $ \s -> case f s of 
                                        (Error e) -> Error e
                                        (Success (a :# s2)) -> case g s2 of 
                                                                  (Error e2) -> Error e2
                                                                  (Success (h :# s3)) -> Success $ a h :# s3

instance Monad (ExceptState e s) where
  (>>=) (ES f) g = ES $ \s -> case f s of
                                  (Error e) -> Error e
                                  (Success (a :# s2)) -> let (ES f2) = g a in f2 s2

data EvaluationError = DivideByZero
  deriving Show

unaryProto :: (Double -> Prim Double) -> (Double -> Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
unaryProto constr conv expr = do
  val <- eval expr
  modifyExceptState $ \s -> constr val : s
  return (conv val)

binaryProto :: (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binaryProto constr conv a b = do
  left <- eval a 
  right <- eval b
  modifyExceptState $ \s -> constr left right : s
  return $ conv left right


eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val v) = pure v

eval (Op (Abs expr)) = unaryProto Abs abs expr

eval (Op (Sgn expr)) = unaryProto Sgn signum expr 

eval (Op (Add a b)) = binaryProto Add (+) a b

eval (Op (Sub a b)) = binaryProto Sub (-) a b

eval (Op (Mul a b)) = binaryProto Mul (*) a b

eval (Op (Div a b)) = do
  left <- eval a
  right <- eval b
  if right == 0 then 
    throwExceptState DivideByZero 
  else do
    modifyExceptState $ \s -> Div left right : s
    return $ left / right


