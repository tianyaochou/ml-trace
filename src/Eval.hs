module Eval (eval) where

import Class
import Expr

eval :: Env -> Expr -> Either String Value
eval _ (Const v) = return v
eval env (Var var) = maybe (Left "Var not found") return (lookup var env)
eval env (Bin op e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  interpBinOp op v1 v2
eval env (Let var e eb) = do
  v <- eval env e
  eval ((var, v) : env) eb
eval env (ProdE e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  return (prod v1 v2)
eval env (Fst e) = do
  v <- eval env e
  case v of
    (ProdV v1 _) -> return v1
    _ -> Left "Not a product"
eval env (Snd e) = do
  v <- eval env e
  case v of
    (ProdV _ v2) -> return v2
    _ -> Left "Not a product"
eval env (InlE e) = fmap inl $ eval env e
eval env (InrE e) = fmap inr $ eval env e
eval env (Match e x e1 y e2) = do
  v <- eval env e
  case v of
    InlV v' -> eval ((x, v') : env) e1
    InrV v' -> eval ((y, v') : env) e2
    _ -> Left "Not a sum"
eval env (Lam var e) = return (Closure env var e)
eval env (Rec fnam arg e) = let x = (Closure ((fnam, x) : env) arg e) in return x
eval env (App ef e) = do
  (capture, var, eb) <- eval env ef >>= fromClosure
  arg <- eval env e
  eval ((var, arg) : capture) eb
eval env (Unfold e) = do
  v <- eval env e
  case v of
    FoldV v' -> return v'
    _ -> Left "Not a recursive type"

fromClosure :: Value -> Either String (Env, Ident, Expr)
fromClosure (Closure env var e) = return (env, var, e)
fromClosure _ = Left "Not a closure"

interpBinOp :: BinOp -> Value -> Value -> Either String Value
interpBinOp op v1 v2 = do
  n1 <- fromValue v1
  n2 <- fromValue v2
  case op of
    Plus -> return $ Num (n1 + n2)
    Mult -> return $ Num (n1 * n2)
    Minus -> return $ Num (n1 - n2)
    Div -> return $ Num (n1 `div` n2)
    Eq -> return $ if n1 == n2 then true else false

fromValue :: Value -> Either String Int
fromValue (Num n) = return n
fromValue _ = Left "Not a number"
