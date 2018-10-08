--module Main where
--import Parser
--import Lexer
import Data.Dynamic
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as Map

{- FOR USE ON REPL.IT -}
{-
 - Nota de modificação: os construtores foram refatorados para uma melhor legibilidade. Agora constutores tem de 1 a 3 letras, e os tipos são escritos por extenso. 
 - Deste modo, o código com coerções (como no autômato) fica bem enxuto, e fica bem claro o que é tipo e o que é construtor na definição dos data's (os tipos)
 - -}
data Statement = E Expression
               | C Command
               | D Declaration
               | K Control deriving Show

data Expression = Ae ArithmeticExpression 
                | Be BooleanExpression 
                | Id Identifier -- será que não é Id String ?
                | Rf Expression -- será que não é Rf Reference ?
                | Dr Identifier
                | Vr Identifier deriving (Show, Eq) 

data ArithmeticExpression = N   Int 
                          | Sum ArithmeticExpression ArithmeticExpression  
                          | Sub ArithmeticExpression ArithmeticExpression
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

data Control = Kw  Keyword
             | Cmd Command deriving (Show, Eq)

data Identifier = I String deriving (Show, Eq, Ord)

data Keyword = KWSum 
             | KWMul 
             | KWSub 
             | KWNot
             | KWAnd
             | KWEq 
             | KWOr 
             | KWLt 
             | KWLe
             | KWGt 
             | KWGe
             | KWAssign 
             | KWLoop
             | KWRef
             | KWCns
             | KWBlkd
             | KWBlc
             | KWBind
             | KWDSeq deriving (Show, Eq)

data Value = Vb  { bval :: Bool } 
           | Vi  { ival :: Int } 
           | Vlp { beval :: BooleanExpression, cmdval :: Command} 
           | Vid { idval :: Identifier } 
           | Vcm { cval :: Command } 
           | Vl  { lval :: Location } deriving (Show, Eq)
{- |
 - Automata definition
 -}
 
data Location = Loc Int 
              | Sto Storable deriving (Show, Eq, Ord) 

type Storable = Either Bool Int 

type ValueStack = [Value]
type ControlStack = [Statement]
type Loc = Integer

data CmdPiAut = CmdPiAut { env :: Map.Map Identifier Location, -- TODO: verificar tipagem, possível confusão entre Location, Identifier, Storable
                           sto :: Map.Map Location Storable, 
                           val :: ValueStack,
                           cnt :: ControlStack,
                           locs :: [Int]
                         } deriving Show

lookup' :: Ord k => k -> Map.Map k a -> a
lookup' k m = fromJust $ Map.lookup k m

-- Right -> int value, Left -> bool value
evalStorable :: Storable -> Value
evalStorable (Right x) = Vi x
evalStorable (Left  x) = Vb x

storeValue :: Value -> Storable
storeValue (Vi x) = Right x
storeValue (Vb x) = Left x

eval :: CmdPiAut -> CmdPiAut
eval cpa@(CmdPiAut {cnt = []}) = cpa
eval cpa@(CmdPiAut e s v c l)  = eval $ case (head c) of
                                      E (Ae (Sum aex1 aex2))   -> cpa{cnt = E (Ae aex1) : E (Ae aex2) : K (Kw KWSum) : tail c} 
                                      E (Ae (Sub aex1 aex2))   -> cpa{cnt = E (Ae aex2) : E (Ae aex1) : K (Kw KWSub) : tail c}
                                      E (Ae (Mul aex1 aex2))   -> cpa{cnt = E (Ae aex1) : E (Ae aex2) : K (Kw KWMul) : tail c}
                                      E (Be (Eq  exp1 exp2))   -> cpa{cnt = E (Be exp1) : E (Be exp2) : K (Kw KWEq)  : tail c} 
                                      E (Be (Or  exp1 exp2))   -> cpa{cnt = E (Be exp1) : E (Be exp2) : K (Kw KWOr)  : tail c} 
                                      E (Be (And exp1 exp2))   -> cpa{cnt = E (Be exp1) : E (Be exp2) : K (Kw KWAnd) : tail c} 
                                      E (Be (Lt  exp1 exp2))   -> cpa{cnt = E (Ae exp2) : E (Ae exp1) : K (Kw KWLt)  : tail c} 
                                      E (Be (Le  exp1 exp2))   -> cpa{cnt = E (Ae exp2) : E (Ae exp1) : K (Kw KWLe)  : tail c} 
                                      E (Be (Ge  exp1 exp2))   -> cpa{cnt = E (Ae exp2) : E (Ae exp1) : K (Kw KWGe)  : tail c} 
                                      E (Be (Gt  exp1 exp2))   -> cpa{cnt = E (Ae exp2) : E (Ae exp1) : K (Kw KWGt)  : tail c} 
                                      E (Be (Not ex))          -> cpa{cnt = E (Be ex) : K (Kw KWNot) : tail c}
                                      E (Ae (N intval))        -> cpa{cnt = tail c, val = Vi intval : v}
                                      E (Be (B booval))        -> cpa{cnt = tail c, val = Vb booval : v}
                                      K (Kw KWNot)             -> cpa{cnt = tail c, val = Vb (not $ bval $ head v) : tail v}
                                      E (Id (I str))           -> cpa{cnt = tail c, val = evalStorable (lookup' (lookup' (I str) e) s) : v} 
                                      C (A idtf exp)           -> cpa{cnt = E exp : K (Kw KWAssign) : tail c, 
                                                                      val = Vid idtf : v} --sto = Map.insert (Loc $ Map.size s + 1) idtf s} segundo implementação em python, semântica formal está confusa
                                      C (Cs cmd1 cmd2)         -> cpa{cnt = C cmd1 : C cmd2 : tail c}
                                      C (L bexp cmd)           -> cpa{cnt = E (Be bexp) : K (Kw KWLoop) : tail c, val = Vlp bexp cmd : v} -- faltando push da bexp antes do Vlp
                                      E (Rf exp)               -> cpa{cnt = E exp : K (Kw KWRef) : tail c}
                                      E (Dr idt)               -> cpa{cnt = tail c, val = Vl (lookup' idt e) : v}
                                      E (Vr idt)               -> cpa{cnt = tail c, val = evalStorable (lookup' (lookup' idt e) s) : v}
                                      D (Ds dec1 dec2 )        -> cpa{cnt = D dec1 : D dec2 : tail c}
                                      x -> let (va:vb:vs) = v in case x of
                                           K (Kw KWSum)   -> cpa{cnt = tail c, val = Vi (ival va + ival vb)  : vs}
                                           K (Kw KWSub)   -> cpa{cnt = tail c, val = Vi (ival va - ival vb)  : vs}
                                           K (Kw KWMul)   -> cpa{cnt = tail c, val = Vi (ival va * ival vb)  : vs}
                                           K (Kw KWEq)    -> cpa{cnt = tail c, val = Vb (bval va == bval vb) : vs}
                                           K (Kw KWLe)    -> cpa{cnt = tail c, val = Vb (ival va >= ival vb) : vs}
                                           K (Kw KWLt)    -> cpa{cnt = tail c, val = Vb (ival va > ival vb)  : vs}
                                           K (Kw KWGe)    -> cpa{cnt = tail c, val = Vb (ival va <= ival vb) : vs}
                                           K (Kw KWGt)    -> cpa{cnt = tail c, val = Vb (ival va < ival vb)  : vs}
                                           K (Kw KWOr)    -> cpa{cnt = tail c, val = Vb (bval va || bval vb) : vs}
                                           K (Kw KWAnd)   -> cpa{cnt = tail c, val = Vb (bval va && bval vb) : vs}
                                           K (Kw KWAssign)-> cpa{cnt = tail c, val = vs, -- esperar erros como se va for loop, identifier ou command
                                                                 sto = Map.insert (lookup' (idval vb) e) (storeValue va) s } 
                                           K (Kw KWLoop)  -> cpa{cnt = if(bval $ va) then (C $ L (beval vb) (cmdval vb)) : tail c 
                                                                                     else tail c ,val = vs}
                                           K (Kw KWRef)   -> let loc = (Map.size s) + 1 in 
                                                             cpa{cnt = tail c, sto = Map.insert (Loc loc) (storeValue va) s, val = Vl (Loc loc) : vs, locs = loc : l}
{-
main :: IO ()
main = do
    --s <- readFile "C:\\Users\\rodri\\OneDrive\\Documentos\\compiladores\\program.txt"
    --print s
    --let ast = parseCalc (scanTokens s)
    let ast = Aexp $ Sum (Num 1) (Num 1)
    --let ast = Comm (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1)))
    --let ast = Comm (CSeq (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1))) (Assign (Id "Meu ID Denovo!") (Aexp $ Sum (Num 1) (Num 1))))
    --let ast = Comm (Loop (Eq (Boo False) (Boo False)) (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1))))
    --let ast = Comm (CSeq (Assign (Id "Meu ID") (Aexp $ Num 1)) (Loop (Lt (Id "Meu ID") (Num 5)) (Assign (Id "Meu ID") (Aexp $ Sum (Id "Meu ID") (Num 1)))))
    let aut = CmdPiAut (Map.fromList []) (Map.fromList []) [] [ast]
    let result =  eval aut
    print ast
    print result
-}
{- Example expressions
Num 2
Sum (Num 1) (Num 1)
Sub (Num 1) (Num 1)
Mul (Num 1) (Num 1)
Boo True
Eq (Boo True) (Boo False)
Not (Boo True)
Gt (Num 2) (Num 1)
Ge (Num 2) (Num 2)
Lt (Num 1) (Num 2)
Le (Num 2) (Num 2)
And (Boo True) (Boo True)
Or (Boo True) (Boo False)
-- atÃ© aqui, tudo OK
Id "Meu ID"
Idtf (Id "Meu ID")
Assign (Id "Meu ID") (Sum (Num 1) (Num 1))
Comm (Assign (Id "Meu ID") (Sum (Num 1) (Num 1)))
Aexp (Sum (Num 1) (Num 1))
Bexp (Eq (Boo True) (Boo False))
Exp (Aexp (Sum (Num 1) (Num 1)))
Command (Assign (Id "Meu ID") (Sum (Num 1) (Num 1)))
Loop (Eq (Boo True) (Boo False)) (Assign (Id "Meu ID") (Sum (Num 1) (Num 1)))
CSeq (Assign (Id "Meu ID") (Sum (Num 1) (Num 1))) (Assign (Id "Meu ID denovo") (Sum (Num 1) (Num 1))
let exp = Sum (Num 5) (Num 2)
-}
