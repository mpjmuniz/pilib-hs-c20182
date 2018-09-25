{
module Tokens where
import Data.Bool
}

%wrapper "basic"

$digit = 0-9			-- digits
tokens :-
  $white+			;
  $digit+			{ \s -> TokenInt(read s)}	
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
  \&&							{\s  -> TokenAnd}
  "||"							{\s  -> TokenOr}  
  False			                { \s -> TokenFalse}
  True					        { \s -> TokenTrue}

{
-- Each action has type :: String -> Token

-- The token type:
data Token = TokenEq
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
		   | TokenTrue
           | TokenFalse	deriving (Eq,Show)

scanTokens = alexScanTokens


--main::IO ()
--main = do
--  s <- getContents
-- x let x = alexScanTokens s
--  mapM_ print x
}