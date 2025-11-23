{
module Lexer (alexScanTokens) where
import Lexeme
import Text.Parsec.Pos
}

%wrapper "posn"

$digit = [0-9]
$alpha = [A-Za-z]
@int = ("-")?$digit+
@name = [a-z][$alpha $digit \_ \']*
@constr = [A-Z][$alpha $digit \_ \']*

tokens :-
	$white+    ;
	"//".*     ;
	type               { \pos s -> mkLexeme (TokenType) (calcRange pos s) }
	"()"               { \pos s -> mkLexeme (TokenUnit) (calcRange pos s) }
	@int               { \pos s -> mkLexeme (TokenNum (read s)) (calcRange pos s) }
	"+"                { \pos s -> mkLexeme (TokenPlus) (calcRange pos s) }
	"-"                { \pos s -> mkLexeme (TokenMinus) (calcRange pos s) }
	"*"                { \pos s -> mkLexeme (TokenMult) (calcRange pos s) }
	"/"                { \pos s -> mkLexeme (TokenDiv) (calcRange pos s) }
	"="                { \pos s -> mkLexeme (TokenEq) (calcRange pos s) }
	"<"                { \pos s -> mkLexeme (TokenLt) (calcRange pos s) }
	">"                { \pos s -> mkLexeme (TokenGt) (calcRange pos s) }
	let                { \pos s -> mkLexeme (TokenLet) (calcRange pos s) }
	in                 { \pos s -> mkLexeme (TokenIn) (calcRange pos s) }
	"("                { \pos s -> mkLexeme (TokenLParen) (calcRange pos s) }
	")"                { \pos s -> mkLexeme (TokenRParen) (calcRange pos s) }
	","                { \pos s -> mkLexeme (TokenComma) (calcRange pos s) }
	match              { \pos s -> mkLexeme (TokenMatch) (calcRange pos s) }
	"_"                { \pos s -> mkLexeme (TokenWild) (calcRange pos s) }
	with               { \pos s -> mkLexeme (TokenWith) (calcRange pos s) }
	"->"               { \pos s -> mkLexeme (TokenArrow) (calcRange pos s) }
	"|"                { \pos s -> mkLexeme (TokenCaseSplit) (calcRange pos s) }
	end                { \pos s -> mkLexeme (TokenEnd) (calcRange pos s) }
	"["                { \pos s -> mkLexeme (TokenLBrack) (calcRange pos s) }
	"]"                { \pos s -> mkLexeme (TokenRBrack) (calcRange pos s) }
	fun                { \pos s -> mkLexeme (TokenFun) (calcRange pos s) }
	rec                { \pos s -> mkLexeme (TokenRec) (calcRange pos s) }
	@name              { \pos s -> mkLexeme (TokenName s) (calcRange pos s) }
	@constr            { \pos s -> mkLexeme (TokenConstr s) (calcRange pos s) }

{
mkLexeme a b = (a, b)
fromAlexPos (AlexPn off line col) = newPos "" line col

calcRange pos s =
  range (fromAlexPos pos) (incSourceColumn (fromAlexPos pos) (length (s :: String)))
}
