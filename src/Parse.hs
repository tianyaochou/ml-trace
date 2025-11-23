module Parse where
import Text.Parsec
import Expr
import Lexeme
import Lexer
import Data.Functor.Identity (Identity)

type LexParsec a = ParsecT [Lexeme] () Identity a

tok :: Token -> LexParsec Lexeme
tok x = token showTok posFromTok testTok
  where
    showTok (t, _) = show t
    posFromTok (_, p) = rStart p
    testTok l@(t, _) = if sameKind x t then Just l else Nothing

numberFromToken :: Token -> LexParsec Int
numberFromToken (TokenNum n) = return n
numberFromToken _ = fail "Not a number"
  

nameFromToken :: Token -> LexParsec String
nameFromToken (TokenName n) = return n
nameFromToken _ = fail "Not a name"

constrFromToken :: Token -> LexParsec String
constrFromToken (TokenConstr n) = return n
constrFromToken _ = fail "Not a constr"

program :: LexParsec [Decl]
program = do
  many decl

decl :: LexParsec Decl
decl = typeDecl <|> varDecl

parseConstr = do
  constr <- fst <$> tok (TokenConstr "")
  case constr of
    TokenConstr n -> return n
    _ -> fail "impossible should be a constructor"

typeDecl :: LexParsec Decl
typeDecl = do
  sPos <- rangeOf <$> tok TokenType
  name <- typeName <$> (constrFromToken =<< tokenOf <$> tok (TokenConstr ""))
  tok TokenEq
  constrs <- sepBy constrDecl (tok TokenCaseSplit)
  return (ADTDecl (ADT name constrs))

constrDecl :: LexParsec Constructor
constrDecl = do
  name <- constrName <$> (constrFromToken =<< tokenOf <$> tok (TokenConstr ""))
  args <- many (Custom . typeName <$> (nameFromToken =<< tokenOf <$> tok (TokenName "")))
  return $ Constructor name args

varDecl :: LexParsec Decl
varDecl = do
  tok TokenLet
  recTok <- optionMaybe $ tok TokenRec
  name <- varName <$> (nameFromToken =<< tokenOf <$> tok (TokenName ""))
  tok TokenEq
  exp <- expr
  case recTok of
    Just _ -> return $ VarDecl True name exp
    Nothing -> return $ VarDecl False name exp

expr :: LexParsec Expr
expr =
  (try letIn)
  <|> (try match)
  <|> (try lam)
  <|> (try arithm)

lam = do
  tokf <- tok TokenFun
  arg <- varName <$> (nameFromToken =<< tokenOf <$> tok (TokenName ""))
  tok TokenArrow
  e <- expr
  return $ LamE arg e (rangeSpan tokf e)

match = do
  tokMatch <- tok TokenMatch
  e <- expr
  tok TokenWith
  cases <- sepBy matchCase (tok TokenCaseSplit)
  tok TokenEnd
  case cases of
    [] -> fail "empty match"
    _ : _ -> return $ MatchE e cases (rangeSpan tokMatch $ matchCaseExpr (last cases))

matchCase = do
  pat <- pattern
  tok TokenArrow
  MatchCase pat <$> expr

pattern = do
  NumP . fst <$> number
  <|> VarP . varName <$> (nameFromToken =<< tokenOf <$> tok (TokenName ""))
  <|> do {tok TokenWild; return WildP}
  <|> ConstrP <$> (constrName <$> (constrFromToken =<< tokenOf <$> tok (TokenConstr ""))) <*> many pattern

letIn :: LexParsec Expr
letIn = do
  tokLet <- tok TokenLet
  recM <- optionMaybe $ tok TokenRec
  name <- varName <$> (nameFromToken =<< tokenOf <$> tok (TokenName ""))
  tok TokenEq
  e <- expr
  tok TokenIn
  eb <- expr
  case recM of
    Just _ -> return $ LetE name e eb (rangeSpan tokLet eb)
    Nothing -> return $ LetE name e eb (rangeSpan tokLet eb)

arithm :: LexParsec Expr
arithm = do
  chainl1 comparee (
    do { tok TokenEq; return $ \a b -> BinE Eq a b (rangeSpan a b) }
    <|> do {tok TokenLt; return $ \a b -> BinE Lt a b (rangeSpan a b) }
    <|> do {tok TokenGt; return $ \a b -> BinE Gt a b (rangeSpan a b) })

comparee = do
  chainl1 term (
    do { tok TokenPlus; return $ \a b -> BinE Plus a b (rangeSpan a b) }
    <|> do { tok TokenMinus; return $ \a b -> BinE Minus a b (rangeSpan a b)})

term = do
  chainl1 app (
    do { tok TokenMult; return $ \a b -> BinE Mult a b (rangeSpan a b) }
    <|> do { tok TokenDiv; return $ \a b -> BinE Div a b (rangeSpan a b) })

app = do
  chainl1 factor (return $ \a b -> AppE a b (rangeSpan a b))

factor = do
  numberE <|> variable <|> constr <|> betweenParen expr

numberE = uncurry NumE <$> number

number :: LexParsec (Int, Range)
number = do
  ntok <- tok $ TokenNum 0
  (,) <$> (numberFromToken $ tokenOf ntok) <*> return (rangeOf ntok)

variable :: LexParsec Expr
variable = do
  ntok <- tok $ TokenName ""
  VarE <$> (varName <$> (nameFromToken $ tokenOf ntok)) <*> return (rangeOf ntok)

constr :: LexParsec Expr
constr = do
  ntok <- tok $ TokenConstr ""
  m <- optionMaybe $ lookAhead $ tok TokenLParen
  let args = case m of
              Nothing -> return []
              Just _ -> betweenParen (sepBy1 expr (tok TokenComma))
  ConstrE <$> (constrName <$> (constrFromToken $ tokenOf ntok)) <*> args <*> return (rangeOf ntok)

betweenParen p = between (tok TokenLParen) (tok TokenRParen) p

tokenOf :: Lexeme -> Token
tokenOf = fst
