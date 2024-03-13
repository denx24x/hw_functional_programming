{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad

import HW4.Types
import HW4.T1 (ExceptState(..))
import Data.Char(isDigit, isSpace, digitToInt)
import GHC.Float (int2Double)

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str = case runES es (0, str) of
                    (Error e) -> Error e
                    (Success (a :# _)) -> Success a

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(n, _) -> Error $ ErrorAtPos n

instance Alternative Parser where
  empty = parseError
  (<|>) (P a) (P b) = P $ ES $ \s -> case runES a s of
                                    (Error _) -> runES b s
                                    success -> success


-- No metohds
instance MonadPlus Parser

pEof :: Parser ()
pEof = P (ES (\s@(n, str) -> case str of
                                [] -> Success (() :# s)
                                _ -> Error (ErrorAtPos n)))

convertLeft :: String -> Int
convertLeft [] = 0
convertLeft (x:xs) = digitToInt x + 10 * convertLeft xs

convertRight :: String -> Double
convertRight [] = 0
convertRight (x:xs) = (int2Double (digitToInt x) + convertRight xs) / 10

pSpecChar :: Char -> Parser ()
pSpecChar char = do
  v <- pChar
  unless (v == char) parseError

pInt :: Parser Expr
pInt = do
  val <- some $ mfilter isDigit pChar
  return $ Val $ int2Double $ convertLeft $ reverse val

pFloat :: Parser Expr
pFloat = do
  left <- some $ mfilter isDigit pChar
  pSpecChar '.'
  right <- some (mfilter isDigit pChar)
  return $ Val $ int2Double (convertLeft (reverse left))  + convertRight right


pVal :: Parser Expr
pVal = msum [pFloat, pInt]

pSkipWhitespace :: Parser ()
pSkipWhitespace = void $ many $ mfilter isSpace pChar

pParentheses :: Parser Expr
pParentheses =  do
  pSpecChar '('
  res <- pExpr
  pSpecChar ')'
  return res

pExprOp :: Expr -> Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr
pExprOp left char constr = do
  --left <- pTerm
  pSkipWhitespace
  pSpecChar char
  pSkipWhitespace
  right <- pExpr
  return $ Op $ constr left right


pTermOp  :: Expr -> Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr
pTermOp left char constr = do
  --left <- pFactor
  pSkipWhitespace
  pSpecChar char
  pSkipWhitespace
  right <- pTerm
  return $ Op $ constr left right


pExpr :: Parser Expr
pExpr = do
  left <- pTerm
  msum [pExprOp left '*' Mul, pExprOp left '/' Div, pure left]

pTerm :: Parser Expr
pTerm = do
  left <- pFactor
  msum [pTermOp left '+' Add, pTermOp left '-' Sub, pure left]

pFactor :: Parser Expr
pFactor = msum [pVal, pParentheses]

pInitExpr :: Parser Expr
pInitExpr = do
  res <- pExpr
  pEof
  return res

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pInitExpr



