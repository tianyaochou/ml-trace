module Lib where

import Class
import Expr

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fromList :: [Value] -> Value
fromList [] = inl TT
fromList (v : t) = cons v (fromList t)

toList :: Value -> Maybe [Value]
toList (InlV TT) = return []
toList (InrV (ProdV v t)) = fmap (v:) (toList t)
toList _ = Nothing

cons h t = inr (prod h t)

mapML :: Expr
mapML =
  Rec "map" "l" $
    Lam "f" $
      Match (Var "l") "_" (inl tt) "cons" (cons (app "f" $ Fst (Var "cons")) (App (app "map" (Snd (Var "cons"))) (Var "f")))

eq = Bin Eq

add = Bin Plus

minus = Bin Minus

ifE c th el = Match c "_" th "_" el

app f a = (App (Var f) a)

fib :: Expr
fib = Rec "fib" "n" (ifE (eq (Var "n") (num 0)) (num 0) (ifE (eq (Var "n") (num 1)) (num 1) (add (app "fib" (minus (Var "n") (num 1))) (app "fib" (minus (Var "n") (num 2))))))
