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
%error     { parseError }

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
    id     {TokenVarId $$}
    var     {TokenVar}
    let     {TokenLet}
    in      {TokenIn}
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

Sttmnt : Expr                    { E $1 }
       | Cmd                     { C $1 }
	   | Dec                     { D $1 }

Expr : Aexpr                     { Ae $1 }
     | Bexpr                     { Be $1 }
     
    	  
Aexpr : '-' Aexpr                  { Mul (N(-1)) $2 }
      | Aexpr '+' Aexpr             { Sum $1 $3 }
      | Aexpr '-' Aexpr            { Sub $1 $3 }
      | Aexpr '*' Aexpr            { Mul $1 $3 }
      | '(' Aexpr ')'              { $2 }
	  | Identifier                { Id $1 }
      | int                        { N $1 }
	
Bexpr : Aexpr '<' Aexpr             { Lt $1 $3 }
      | Aexpr '>' Aexpr             { Gt $1 $3 }
      | Aexpr '<=' Aexpr            { Le $1 $3 }
      | Aexpr '>=' Aexpr            { Ge $1 $3 }
      | Bexpr '==' Bexpr           { Eq $1 $3 }
      | Bexpr '&&' Bexpr           { And $1 $3 }
      | Bexpr '||' Bexpr           { Or $1 $3 }
      | '(' Bexpr ')'              { $2 }
      | true                       { B True }
      | false                      { B False }
	  

Cmd : Identifier ':=' Expr                   {A $1 $3} 
    | while '(' Bexpr ')' do '{' Cmd '}'     {L $3 $7}
    | Cmd ';' Cmd                            {Cs $1 $3}
	| let Dec in Cmd                         {Bl $2 $4}

Identifier:  id                   {I $1}

Dec:  var Identifier ':=' Expr        {Bi $2 $4}
     |Dec ';' Dec                     {Ds $1 $3}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Control = S Statement | K Keyword deriving (Show, Eq)

data Statement = E Expression
               | C Command
               | D Declaration deriving (Show, Eq)

data Expression = Ae ArithmeticExpression 
                | Be BooleanExpression                
                | Rf Expression -- ser� que n�o � Rf Reference ?
                | Dr Identifier
                | Vr Identifier deriving (Show, Eq) 

data ArithmeticExpression = N   Int 
                          | Sum ArithmeticExpression ArithmeticExpression  
                          | Sub ArithmeticExpression ArithmeticExpression
			  | Id Identifier -- ser� que n�o � Id String ?
                          | Mul ArithmeticExpression ArithmeticExpression deriving (Show, Eq)

data BooleanExpression = B   Bool 
                       | Not BooleanExpression 
                       | Eq  BooleanExpression    BooleanExpression 
                       | And BooleanExpression    BooleanExpression  
                       | Or  BooleanExpression    BooleanExpression                     					   
                       | Gt  ArithmeticExpression ArithmeticExpression 
                       | Ge  ArithmeticExpression ArithmeticExpression
		       | Lt  ArithmeticExpression ArithmeticExpression
                       | Le  ArithmeticExpression ArithmeticExpression deriving (Show, Eq)

data Command = A  Identifier Expression
             | L  BooleanExpression Command
             | Cs Command Command 
             | Bl Declaration Command deriving (Show, Eq)

data Declaration = Bi Identifier Expression
                 | Ds Declaration Declaration deriving (Show, Eq)

data Identifier = I String deriving (Show, Eq, Ord)

data Keyword = KWSum | KWMul | KWSub | KWNot | KWAnd | KWEq | KWOr | KWLt | KWLe | KWGt | KWGe
             | KWAssign | KWLoop | KWRef
             | KWCns | KWDec | KWBlk | KWBind | KWDSeq deriving (Show, Eq)
 
data Location = Loc Int | Sto Storable deriving (Show, Eq, Ord) 
type Storable = Either Bool Int 

data Value = Vb  { bval :: Bool } 
           | Vi  { ival :: Int } 
           | Vlp { beval :: BooleanExpression, cmdval :: Command} 
           | Vid { idval :: Identifier } 
           | Vcm { cval :: Command } 
           | Vl  { lval :: Location } 
           | Lvls{ lvals :: [Location] } 
           | Bng { xval :: Expression, itval :: Identifier} 
           | Env { enval :: Map.Map Identifier Location} 
           | En  { lcval :: Location, idtval :: Identifier} deriving (Show, Eq)

}
