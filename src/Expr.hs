{-# LANGUAGE GADTs #-}

module Expr where

import Text.Parsec (SourcePos)
import Lexeme

data Name = Name {ident :: String, kind :: NameKind} deriving (Show, Eq)

data NameKind = VarName | TypeName | ConstrName deriving (Show, Eq)

varName :: String -> Name
varName name = Name name VarName

typeName :: String -> Name
typeName name = Name name TypeName

constrName :: String -> Name
constrName name = Name name ConstrName

data Type
  = Int
  | Custom Name
  deriving (Show)

data ADT
  = ADT Name [Constructor]
  deriving (Show)

data Decl = ADTDecl ADT | VarDecl Bool Name Expr
  deriving (Show)

data Constructor
  = Constructor {name_con :: Name, args :: [Type]}
  deriving (Show)

data Pattern
  = NumP Int
  | VarP Name
  | WildP
  | ConstrP Name [Pattern]
  deriving (Show)

data MatchCase = MatchCase Pattern Expr
  deriving (Show)

matchCaseExpr (MatchCase p e) = e
casePattern (MatchCase p e) = p

-- Value
data Value where
  NumV :: Int -> Value
  ConstrV :: Name -> [Value] -> Value
  ClosureV :: TraceEnv -> Name -> Expr -> Value
  -- Trace :: Trace -> Value
  deriving (Show)

-- Trace
data Trace where
  ConstT :: Value -> Range -> Trace
  VarT :: Name -> Trace -> Range -> Trace
  ConstrT :: Name -> [Trace] -> Range -> Trace
  BinT :: BinOp -> Trace -> Trace -> Value -> Trace
  LetT :: Bool -> Name -> Trace -> Trace -> Trace
  MatchT :: Trace -> [MatchCase] -> Pattern -> Trace -> [MatchCase] -> Trace
  -- Function
  ClosureT :: TraceEnv -> Name -> Expr -> Trace
  AppT :: Trace -> Trace -> Trace -> Trace
  deriving (Show)

value :: Trace -> Value
value (ConstT v _) = v
value (VarT _ t _) = value t
value (ConstrT name ts _) = ConstrV name (value <$> ts)
value (BinT _ _ _ v) = v
value (LetT _ _ _ tb) = value tb
value (MatchT _ _ _ ts _) = value ts
value (ClosureT env n e) = ClosureV env n e
value (AppT f a r) = value r

type TraceEnv = [(String, Trace)]

isClosure :: Value -> Bool
isClosure (ClosureV _ _ _) = True
isClosure _ = False

-- Expr

data BinOp = Plus | Mult | Minus | Div | Eq | Lt | Gt
instance Show BinOp where
  show Plus = "+"
  show Mult = "*"
  show Minus = "-"
  show Div = "/"
  show Eq = "="
  show Lt = "<"
  show Gt = ">"

type Args = Name

data Expr
  = NumE Int Range
  | VarE Name Range
  | BinE BinOp Expr Expr Range
  | LetE Name Expr Expr Range -- let <var> = <expr> in <expr>
  -- ADT
  | ConstrE Name [Expr] Range
  | MatchE Expr [MatchCase] Range -- match <expr> with inl <var> -> <expr> | inr <var> -> <expr> end
  -- Function
  | LamE Args Expr Range -- lam <var> -> <expr>
  | AppE Expr Expr Range
  deriving (Show)

instance HasRange Expr where
  rangeOf (NumE _ r) = r
  rangeOf (VarE _ r) = r
  rangeOf (BinE _ _ _ r) = r
  rangeOf (LetE _ _ _ r) = r
  rangeOf (ConstrE _ _ r) = r
  rangeOf (MatchE _ _ r) = r
  rangeOf (LamE _ _ r) = r
  rangeOf (AppE _ _ r) = r
