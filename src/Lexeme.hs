{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lexeme where

import Text.Parsec

type Range = (SourcePos, SourcePos)
class HasRange a where
  rangeOf :: a -> Range

instance HasRange Lexeme where
  rangeOf = snd

rangeSpan :: (HasRange a, HasRange b) => a -> b -> Range
rangeSpan a b = range (rStart $ rangeOf a)  (rEnd $ rangeOf b)

range :: a -> b -> (a, b)
range start end = (start, end)

rStart (a, b) = a
rEnd (a, b) = b

type Lexeme = (Token, Range)

data Token
  = TokenType
  | TokenUnit
  | TokenNum Int
  | TokenName String
  | TokenWild
  | TokenConstr String
  | TokenPlus
  | TokenMinus
  | TokenMult
  | TokenDiv
  | TokenEq
  | TokenLt
  | TokenGt
  | TokenLet
  | TokenIn
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenMatch
  | TokenWith
  | TokenArrow
  | TokenCaseSplit
  | TokenEnd
  | TokenLBrack
  | TokenRBrack
  | TokenFun
  | TokenRec
  deriving Eq

sameKind :: Token -> Token -> Bool
sameKind (TokenNum _) (TokenNum _) = True
sameKind (TokenName _) (TokenName _) = True
sameKind (TokenConstr _) (TokenConstr _) = True
sameKind a b = a == b

instance Show Token where
  show TokenType = "type"
  show TokenUnit = "()"
  show (TokenNum n) = show n
  show (TokenName s) = s
  show (TokenConstr s) = s
  show TokenPlus = "+"
  show TokenMinus = "-"
  show TokenMult = "*"
  show TokenDiv = "/"
  show TokenEq = "="
  show TokenLt = "<"
  show TokenGt = ">"
  show TokenLet = "let"
  show TokenIn = "in"
  show TokenLParen = "("
  show TokenRParen = ")"
  show TokenComma = ","
  show TokenMatch = "match"
  show TokenWith = "with"
  show TokenArrow = "->"
  show TokenCaseSplit = "|"
  show TokenEnd = "end"
  show TokenLBrack = "["
  show TokenRBrack = "]"
  show TokenFun = "fun"
  show TokenRec = "rec"
