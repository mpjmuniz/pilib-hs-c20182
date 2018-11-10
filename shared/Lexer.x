{
module Lexer where
import Data.Bool
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$whitechar = [\s\n\r]

tokens :-
  $white+			;
  $digit+                       { \s -> TokenInt(read s)}	
  ":="                          { \s -> TokenAssign}
  \==                           { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \!                            { \s -> TokenNot }
  \>                            { \s -> TokenMaior }
  \<                            { \s -> TokenMenor }
  \>=                           { \s -> TokenMaiorIgual }
  \<=                           { \s -> TokenMenorIgual }
  \&&                           { \s  -> TokenAnd}
  "||"	                        { \s  -> TokenOr}
  while                         { \s  -> TokenWhile}
  do                            { \s  -> TokenDo}
  \{                            { \s  -> TokenLBrace}
  \}                            { \s  -> TokenRBrace}
  \;                            { \s   -> TokenSemiComma}
  False	                        { \s -> TokenFalse}
  True	                        { \s -> TokenTrue}
  $alpha[$alpha $digit \_]*	{ \s -> TokenVarId s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token = TokenVarId String
           | TokenAssign
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenNot 
           | TokenInt Int
           | TokenMaior
           | TokenMenor
           | TokenMaiorIgual
           | TokenMenorIgual
           | TokenAnd
           | TokenOr
           | TokenWhile
           | TokenDo
           | TokenLBrace
           | TokenRBrace
           | TokenSemiComma
           | TokenTrue
           | TokenFalse deriving (Eq,Show)

scanTokens = alexScanTokens

{-
main::IO ()
main = do
  s <- getContents
  let x = alexScanTokens s
  mapM_ print x
-}
} 
