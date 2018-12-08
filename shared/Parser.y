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

Sttmnt : Expr                       { E $1 }
       | Cmd                        { C $1 }
       | Dec                        { D $1 }

Expr : Aexpr                        { Ae $1 }
     | Bexpr                        { Be $1 }
     
    	  
Aexpr : '-' Aexpr                   { Mul (N(-1)) $2 }
      | Aexpr '+' Aexpr             { Sum $1 $3 }
      | Aexpr '-' Aexpr             { Sub $1 $3 }
      | Aexpr '*' Aexpr             { Mul $1 $3 }
      | '(' Aexpr ')'               { $2 }
	  | Identifier                  { Id $1 }
      | int                         { N $1 }
	
Bexpr : Aexpr '<' Aexpr             { Lt $1 $3 }
      | Aexpr '>' Aexpr             { Gt $1 $3 }
      | Aexpr '<=' Aexpr            { Le $1 $3 }
      | Aexpr '>=' Aexpr            { Ge $1 $3 }
      | Bexpr '==' Bexpr            { Eq $1 $3 }
      | Bexpr '&&' Bexpr            { And $1 $3 }
      | Bexpr '||' Bexpr            { Or $1 $3 }
      | '(' Bexpr ')'               { $2 }
      | true                        { B True }
      | false                       { B False }
	  

Reference: Expr                     {Rf $1}
	  
Cmd : Identifier ':=' Expr                   {A $1 $3} 
    | while '(' Bexpr ')' do '{' Cmd '}'     {L $3 $7}
    | Cmd ';' Cmd                            {Cs $1 $3}
	| let Dec in Cmd                         {Bl $2 $4}

Identifier:  id                   {I $1}

Dec:  var Identifier ':=' Reference   {Bi $2 $4}
     |Dec ';' Dec                     {Ds $1 $3}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Control = Ab Abstraction | S Statement | K Keyword deriving (Show, Eq, Ord)

data Statement = E Expression
               | C Command
               | D Declaration deriving (Show, Eq, Ord)

data Expression = Ae ArithmeticExpression 
                | Be BooleanExpression                
                | Rf Expression -- será que não é Rf Reference ?
                | Dr Identifier
                | Vr Identifier deriving (Show, Eq, Ord) 

data ArithmeticExpression = N   Int 
                          | Sum ArithmeticExpression ArithmeticExpression  
                          | Sub ArithmeticExpression ArithmeticExpression
                          | Id Identifier -- será que não é Id String ?
                          | Mul ArithmeticExpression ArithmeticExpression deriving (Show, Eq, Ord)

data BooleanExpression = B   Bool 
                       | Not BooleanExpression 
                       | Eq  BooleanExpression    BooleanExpression 
                       | And BooleanExpression    BooleanExpression  
                       | Or  BooleanExpression    BooleanExpression                     					   
                       | Gt  ArithmeticExpression ArithmeticExpression 
                       | Ge  ArithmeticExpression ArithmeticExpression
                       | Lt  ArithmeticExpression ArithmeticExpression
                       | Le  ArithmeticExpression ArithmeticExpression deriving (Show, Eq, Ord)

data Command = A  Identifier Expression
             | L  BooleanExpression Command
             | Cs Command Command 
             | Bl { decl :: Declaration, kmd :: Command }
             | Call Identifier [Expression] deriving (Show, Eq, Ord)

data Declaration = None
                 | Bi Identifier Expression
                 | Ds Declaration Declaration
                 | Bn Identifier Abstraction deriving (Show, Eq, Ord)

data Abstraction = Abs [Identifier] Command deriving (Show, Eq, Ord)

data Closure = Clj { formals :: [Identifier], blk :: Command, local :: Environment} 
             | Cls { abstraction :: Abstraction, context :: Environment } deriving (Show, Eq, Ord)

data Identifier = I String deriving (Show, Eq, Ord)

data Keyword = KWSum | KWMul | KWSub | KWNot | KWAnd | KWEq | KWOr | KWLt | KWLe | KWGt | KWGe
             | KWAssign | KWLoop | KWRef
             | KWCns | KWDec | KWBlk | KWBind | KWDSeq
             | KWCall Identifier Int | KWBindAbs deriving (Show, Eq, Ord)
 
type Location = Int 
data Bindable = Lo { loc :: Location } | Sto Storable | Cl { cl :: Closure} deriving (Show, Eq, Ord)
data Storable = Sbool Bool | Sint Int deriving (Show, Eq, Ord)

type Environment = Map.Map Identifier Bindable 
type Store = Map.Map Location Storable

data Value = Vb  { bval :: Bool } 
           | Vi  { ival :: Int } 
           | Vlp { beval :: BooleanExpression, cmdval :: Command} 
           | Vid { idval :: Identifier } 
           | Vcm { cval :: Command } 
           | Vbi { bival :: Bindable } 
           | Vls { lvls :: [Int] } 
           | Lvls{ lvals :: [Location] } -- TODO: change to Vlvls
           | Bng { xval :: Expression, itval :: Identifier} -- TODO: change to Vbng
           | Env { enval :: Environment} -- TODO: change to Venv
           | En  { lcval :: Location, idtval :: Identifier}  -- TODO: change to Ven
           | Vclj{ cljval :: Closure } 
           | Vabs{ absval :: Abstraction } deriving (Show, Eq)

}
