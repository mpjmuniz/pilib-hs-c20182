{
module Parser where
import Lexer
import Data.Typeable
import Data.Maybe
import Data.Dynamic
import qualified Data.Map.Strict as Map
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int     {TokenInt $$}
	true    {TokenTrue}
	false   {TokenFalse}
    '=='    {TokenEq}
    '+'     {TokenPlus}
    '-'     {TokenMinus}
    '*'     {TokenTimes}
    '/'     {TokenDiv}
    '('     {TokenLParen}
    ')'     {TokenRParen}
	'>'     {TokenMaior}
	'<'     {TokenMenor}
	'>='    {TokenMaiorIgual}
	'<='    {TokenMenorIgual}
	'&&'    {TokenAnd}
	'||'    {TokenOr}
	':='    {TokenAssign}
	var     {TokenVarId $$}
	while   {TokenWhile}
	do      {TokenDo}
	'{'     {TokenLBrace}
	'}'     {TokenRBrace}
	';'     {TokenSemiComma}

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%
Expr:  Aexpr                     { Aexp $1 }
      |Bexpr                     { Bexp $1 }
	  |Cmd                       { Comm $1 }
      |Identifier                { Idtf $1 }
    	  
Aexpr : '-' Aexpr                { Mul (Num(-1)) $2 }
    | Aexpr '+' Aexpr            { Sum $1 $3 }
    | Aexpr '-' Aexpr            { Sub $1 $3 }
    | Aexpr '*' Aexpr            { Mul $1 $3 }
    | '(' Aexpr ')'              { $2 }
    | int                        { Num $1 }
	
Bexpr : Aexpr '<' Aexpr          { Lt $1 $3 }
    | Aexpr '>' Aexpr            { Gt $1 $3 }
    | Aexpr '<=' Aexpr           { Le $1 $3 }
    | Aexpr '>=' Aexpr           { Ge $1 $3 }
    | Bexpr '==' Bexpr           { Eq $1 $3 }
    | Bexpr '&&' Bexpr           { And $1 $3 }
	| Bexpr '||' Bexpr           { Or $1 $3 }
    | '(' Bexpr ')'              { $2 }
    | true                       { Boo True }
	| false                      { Boo False }

Cmd :    Identifier ':=' Expr                       {Assign $1 $3} 
        | while '(' Bexpr ')' do '{' Cmd '}'        {Loop $3 $7}
        | Cmd ';' Cmd                               {CSeq $1 $3}

Identifier:  var                  {Id $1}

		
{


parseError :: [Token] -> a
parseError _ = error "Parse error"



data Expr = Aexp Aexpr 
        | Bexp Bexpr 
        | Idtf Identifier 
        | Kw Keyword
        | Comm Cmd deriving Show
		 
data Aexpr = Num Int 
        | Sum Aexpr Aexpr 
        | Sub Aexpr Aexpr 
        | Mul Aexpr Aexpr deriving Show

data Bexpr = Boo Bool 
        | Eq Bexpr Bexpr 
        | Not Bexpr 
		| Gt Aexpr Aexpr 
		| Ge Aexpr Aexpr 
		| Lt Aexpr Aexpr 
		| Le Aexpr Aexpr
		| And Bexpr Bexpr 
		| Or Bexpr Bexpr deriving Show


data Statement = Exp Expr 
        | Command Cmd deriving Show

data Cmd = Assign Identifier Expr 
        | Loop Bexpr Cmd 
        | CSeq Cmd Cmd deriving Show

data Keyword = KWSum 
        | KWMul 
        | KWSub 
        | KWEq 
        | KWNot
        | KWOr 
        | KWAnd
        | KWLt 
        | KWLe
        | KWGt 
        | KWGe
        | KWAssign 
        | KWLoop deriving Show

data Identifier = Id String deriving (Show, Eq, Ord)

data Value = Bo { bval :: Bool } 
        | In { ival :: Int } 
        | Idt { idval :: Identifier } 
        | Lp {beval :: Bexpr, cmdval :: Cmd} 
        | Comd {cval :: Cmd } deriving Show
		

}