{
module Grammar where
import Tokens
import Data.Typeable
import Data.Maybe
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    int { TokenInt $$ }
	true {TokenTrue }
	false {TokenFalse}
    '==' { TokenEq }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    '(' { TokenLParen }
    ')' { TokenRParen }
	'>' {TokenMaior}
	'<' {TokenMenor}
	'>=' {TokenMaiorIgual}
	'<=' {TokenMenorIgual}
	'&&' {TokenAnd}
	'||' {TokenOr}

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%
Expr:  Aexpr                     { Aexp $1 }
      |Bexpr                     { Bexp $1 }
    	  
Aexpr : Aexpr '+' Aexpr          { Sum $1 $3 }
    | Aexpr '-' Aexpr            { Sub $1 $3 }
    | Aexpr '*' Aexpr            { Mul $1 $3 }
    | '(' Aexpr ')'              { $2 }
    | int                        { Num $1 }
	
Bexpr : Aexpr '<' Aexpr            { Lt $1 $3 }
    | Aexpr '>' Aexpr            { Gt $1 $3 }
    | Aexpr '<=' Aexpr           { Le $1 $3 }
    | Aexpr '>=' Aexpr           { Ge $1 $3 }
    | Bexpr '==' Bexpr           { Eq $1 $3 }
    | Bexpr '&&' Bexpr           { And $1 $3 }
	| Bexpr '||' Bexpr           { Or $1 $3 }
    | '(' Bexpr ')'              { $2 }
    | true                       { Boo True }
	| false                      { Boo False }

{


parseError :: [Token] -> a
parseError _ = error "Parse error"



data Identifier = Id String deriving (Show, Eq)

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

data Value = Bo { bval :: Bool } 
        | In { ival :: Int } 
        | Idt { idval :: Identifier } 
        | Lp {beval :: Bexpr, cmdval :: Cmd} 
        | Comd {cval :: Cmd } deriving Show
}