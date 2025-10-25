{-# LANGUAGE GADTs #-}

module Expr where

import Class

-- Value domain

data Value where
  Num :: Int -> Value
  TT :: Value
  InlV :: Value -> Value
  InrV :: Value -> Value
  ProdV :: Value -> Value -> Value
  FoldV :: Value -> Value
  Closure :: Env -> Ident -> Expr -> Value
  deriving (Show)

true :: (HasUnit a, HasInjection a) => a
true = inl tt

false :: (HasUnit a, HasInjection a) => a
false = inr tt

instance HasNum Value where
  num n = Num n

instance HasUnit Value where
  tt = TT

instance HasProd Value where
  prod = ProdV

instance HasInjection Value where
  inl = InlV
  inr = InrV

type Ident = String

type Env = [(Ident, Value)]

isClosure :: Value -> Bool
isClosure (Closure _ _ _) = True
isClosure _ = False

data BinOp = Plus | Mult | Minus | Div | Eq
  deriving (Show)

data Expr where
  Const :: Value -> Expr
  Var :: Ident -> Expr
  Bin :: BinOp -> Expr -> Expr -> Expr
  Let :: Ident -> Expr -> Expr -> Expr
  -- Product
  ProdE :: Expr -> Expr -> Expr
  Fst :: Expr -> Expr
  Snd :: Expr -> Expr
  -- Sum
  InlE :: Expr -> Expr
  InrE :: Expr -> Expr
  Match :: Expr -> Ident -> Expr -> Ident -> Expr -> Expr
  -- Function
  Lam :: Ident -> Expr -> Expr
  Rec :: Ident -> Ident -> Expr -> Expr
  App :: Expr -> Expr -> Expr
  -- Recursive Type
  Unfold :: Expr -> Expr
  deriving (Show)

instance HasUnit Expr where
  tt = Const TT

instance HasNum Expr where
  num n = Const $ Num n

instance HasProd Expr where
  prod = ProdE

instance HasInjection Expr where
  inl = InlE
  inr = InrE
