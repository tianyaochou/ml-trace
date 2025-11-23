module Trace where

import Expr
import Control.Monad (forM)
import Data.Maybe (isNothing)

failE = Left

type TopEnv = [(String, Expr)]

topdecls :: [Decl] -> TopEnv
topdecls [] = mempty
topdecls ((VarDecl _ name e):t) = ((ident name, e):(topdecls t))
topdecls (_: t) = topdecls t

trace :: TopEnv -> TraceEnv -> Expr -> Either String Trace
trace tops env (NumE n r) = return $ ConstT (NumV n) r
trace tops env (VarE n r) = case lookup (ident n) env of
  Just t -> return $ VarT n t r
  Nothing -> case lookup (ident n) tops of
              Just e -> VarT n <$> (trace tops env e) <*> return r
              Nothing -> failE $ "Cannot find var" <> (ident n)
trace tops env (BinE op a b r) = do
  ta <- trace tops env a
  tb <- trace tops env b
  BinT op ta tb <$> interpOp op (value ta) (value tb)
trace tops env (LetE n e eb r) = do
  te <- trace tops env e
  tb <- trace tops (extend env n te) eb
  return $ LetT False n te tb
trace tops env (ConstrE n args r) =
  ConstrT n <$> forM args (\arg -> trace tops env arg) <*> return r
trace tops env (MatchE e cases r) = do
  t <- trace tops env e
  (binding, patMatch, e, cases', cases'') <- matchCases t cases
  tMatch <- trace tops (extendBinding env binding) e
  return $ MatchT t cases' patMatch tMatch cases''
trace tops env (LamE n e r) = return $ ClosureT env n e
trace tops env (AppE f arg r) = do
  tf <- trace tops env f
  targ <- trace tops env arg
  tapp <- apply (value tf) targ
  return $ AppT tf targ tapp
  where
    apply (ClosureV cap n e) targ = trace tops (extend (extendBinding env cap) n targ) e
    apply _ _ = failE "Not a function"

matchCases :: Trace -> [MatchCase] -> Either String (TraceEnv, Pattern, Expr, [MatchCase], [MatchCase])
matchCases t cases =
  let matches = map (\c -> (c, matchT (casePattern c) t)) cases
      (fails, rest) = span (isNothing . snd) matches
  in case rest of
      [] -> failE "No match"
      (c, Just env): others -> return (env, casePattern c, matchCaseExpr c, fst <$> fails, fst <$> others)
      _ -> failE "Impossible"

matchT :: Pattern -> Trace -> Maybe TraceEnv
matchT p t = matchV p (value t)
  where
    matchV (VarP n) _ = return $ extend mempty n t
    matchV (WildP) _ = return mempty
    matchV (NumP n) (NumV m) = if n == m then return mempty else Nothing
    matchV (ConstrP name pats) (ConstrV name' vs) =
      if name == name' then
        mconcat <$> (sequence $ map (\(pat, v) -> matchV pat v) (zip pats vs))
      else
        Nothing
    matchV _ _ = Nothing

extendBinding :: TraceEnv -> TraceEnv -> TraceEnv
extendBinding = (++)

extend :: TraceEnv -> Name -> Trace -> TraceEnv
extend env n t = (ident n, t) : env

true = ConstrV (constrName "True") []
false = ConstrV (constrName "False") []

interpOp :: BinOp -> Value -> Value -> Either String Value
interpOp Plus (NumV m) (NumV n) = return (NumV $ m + n)
interpOp Minus (NumV m) (NumV n) = return (NumV $ m - n)
interpOp Mult (NumV m) (NumV n) = return (NumV $ m * n)
interpOp Div (NumV m) (NumV n) = return (NumV $ div m n)
interpOp Lt (NumV m) (NumV n) = if m < n then return true else return false
interpOp Gt (NumV m) (NumV n) = if m > n then return true else return false
interpOp Eq (NumV m) (NumV n) = if m == n then return true else return false
interpOp _ _ _ = failE "Not a number"
