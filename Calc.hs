module Calc where
import Parser
import Lexer
import Data.Dynamic
import Data.Maybe
import System.IO
import qualified Data.Map.Strict as Map

{- FOR USE ON REPL.IT -}
{-
 - Nota de modificação: os construtores foram refatorados para uma melhor legibilidade. Agora constutores tem de 1 a 3 letras, e os tipos são escritos por extenso. 
 - Deste modo, o código com coerções (como no autômato) fica bem enxuto, e fica bem claro o que é tipo e o que é construtor na definição dos data's (os tipos)
data Control = S Statement | K Keyword deriving Show

data Statement = E Expression
               | C Command
               | D Declaration deriving Show

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

data Identifier = I String deriving (Show, Eq, Ord)

data Keyword = KWSum | KWMul | KWSub | KWNot | KWAnd | KWEq | KWOr | KWLt | KWLe | KWGt | KWGe
             | KWAssign | KWLoop | KWRef
             | KWCns | KWBlkd | KWBlc | KWBind | KWDSeq deriving (Show, Eq)
 
data Value = Vb  { bval :: Bool } 
           | Vi  { ival :: Int } 
           | Vlp { beval :: BooleanExpression, cmdval :: Command} 
           | Vid { idval :: Identifier } 
           | Vcm { cval :: Command } 
           | Vl  { lval :: Location }
           | Bng { itval :: Identifier, xval :: Expression} deriving (Show, Eq) -- só um chute, não entendi bem a especificação formal aqui
 - -}
{- |
 - Automata definition
 -}
 
--data Location = Loc Int | Sto Storable deriving (Show, Eq, Ord) 
--type Storable = Either Bool Int 
type ValueStack = [Value]
type ControlStack = [Control]

data CmdPiAut = CmdPiAut { env :: Map.Map Identifier Location, -- TODO: verificar tipagem, possível confusão entre Location, Identifier, Storable
                           sto :: Map.Map Location Storable, 
                           val :: ValueStack,
                           cnt :: ControlStack,
                           locs :: [Int]
                         } deriving (Show, Eq)

lookup' :: Ord k => k -> Map.Map k a -> a
lookup' k m = fromJust $ Map.lookup k m

-- Right -> int value, Left -> bool value
evalStorable :: Storable -> Value
evalStorable (Right x) = Vi x
evalStorable (Left  x) = Vb x

storeValue :: Value -> Storable
storeValue (Vi x) = Right x
storeValue (Vb x) = Left x

{-
 - Evaluation function. Given an automata, it recursively evaluates the expression in it's control stack
 -}
{-
evalExp :: DeclPiAut -> DeclPiAut
eval cpa@(DeclPiAut e s v c l)
-}
eval :: CmdPiAut -> CmdPiAut
eval cpa@(CmdPiAut {cnt = []}) = cpa
eval cpa@(CmdPiAut e s v c l)  = eval $ case (head c) of
                                      S (E (Ae (Sum aex1 aex2)))   -> cpa{cnt = S (E (Ae aex1)) : S (E (Ae aex2)) : K KWSum : tail c} 
                                      S (E (Ae (Sub aex1 aex2)))   -> cpa{cnt = S (E (Ae aex2)) : S (E (Ae aex1)) : K KWSub : tail c}
                                      S (E (Ae (Mul aex1 aex2)))   -> cpa{cnt = S (E (Ae aex1)) : S (E (Ae aex2)) : K KWMul : tail c}
                                      S (E (Be (Eq  exp1 exp2)))   -> cpa{cnt = S (E (Be exp1)) : S (E (Be exp2)) : K KWEq  : tail c} 
                                      S (E (Be (Or  exp1 exp2)))   -> cpa{cnt = S (E (Be exp1)) : S (E (Be exp2)) : K KWOr  : tail c} 
                                      S (E (Be (And exp1 exp2)))   -> cpa{cnt = S (E (Be exp1)) : S (E (Be exp2)) : K KWAnd : tail c} 
                                      S (E (Be (Lt  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp1)) : S (E (Ae exp2)) : K KWLt  : tail c} 
                                      S (E (Be (Le  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp2)) : S (E (Ae exp1)) : K KWLe  : tail c} 
                                      S (E (Be (Ge  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp2)) : S (E (Ae exp1)) : K KWGe  : tail c} 
                                      S (E (Be (Gt  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp1)) : S (E (Ae exp2)) : K KWGt  : tail c} 
                                      S (E (Be (Not ex)))          -> cpa{cnt = S (E (Be ex))   : K KWNot : tail c}
                                      S (E (Ae (N intval)))        -> cpa{cnt = tail c, val = Vi intval : v}
                                      S (E (Be (B booval)))        -> cpa{cnt = tail c, val = Vb booval : v}
                                      S (C (A idtf exp))           -> cpa{cnt = S (E exp) : K KWAssign : tail c, val = Vid idtf : v} 
--sto = Map.insert (Loc $ Map.size s + 1) idtf s} segundo implementação em python, semântica formal está confusa
                                      S (C (Cs cmd1 cmd2))         -> cpa{cnt = S (C cmd1) : S (C cmd2) : tail c}
                                      S (C (L bexp cmd))           -> cpa{cnt = S (E (Be bexp)) : K KWLoop : tail c, val = Vlp bexp cmd : v} -- faltando push da bexp antes do Vlp
                                      S (E (Rf exp))               -> cpa{cnt = S (E exp) : K KWRef : tail c}
                                      S (E (Dr idt))               -> cpa{cnt = tail c, val = Vl (lookup' idt e) : v}
                                      S (E (Vr idt))               -> cpa{cnt = tail c, val = evalStorable (lookup' (lookup' idt e) s) : v}
                                      S (D (Ds dec1 dec2))         -> cpa{cnt = S (D dec1) : S (D dec2) : tail c}
                                      K KWNot                      -> cpa{cnt = tail c, val = Vb (not $ bval $ head v) : tail v}
                                      S (E (Id id))                -> cpa{cnt = tail c, val = evalStorable (lookup' (lookup' id e) s) : v} 
                                      S (D (Bi id exp))            -> cpa{cnt = S (E exp) : K KWBind : tail c, val = Vid id : v}
                                      S (C (Bl decl cmd))          -> cpa{cnt = S (D decl) : K KWDec : S (C cmd) : tail c, val = Vls l : v, locs = []}
                                      x -> let (va:vb:vs) = v in case x of
                                           K KWSum               -> cpa{cnt = tail c, val = Vi (ival va + ival vb)  : vs}
                                           K KWSub               -> cpa{cnt = tail c, val = Vi (ival va - ival vb)  : vs}
                                           K KWMul               -> cpa{cnt = tail c, val = Vi (ival va * ival vb)  : vs}
                                           K KWEq                -> cpa{cnt = tail c, val = Vb (bval va == bval vb) : vs}
                                           K KWLe                -> cpa{cnt = tail c, val = Vb (ival va >= ival vb) : vs}
                                           K KWLt                -> cpa{cnt = tail c, val = Vb (ival va > ival vb)  : vs}
                                           K KWGe                -> cpa{cnt = tail c, val = Vb (ival va <= ival vb) : vs}
                                           K KWGt                -> cpa{cnt = tail c, val = Vb (ival va < ival vb)  : vs}
                                           K KWOr                -> cpa{cnt = tail c, val = Vb (bval va || bval vb) : vs}
                                           K KWAnd               -> cpa{cnt = tail c, val = Vb (bval va && bval vb) : vs}
                                           K KWAssign            -> cpa{cnt = tail c, val = vs, sto = Map.insert (lookup' (idval vb) e) (storeValue va) s } 
-- esperar erros como se va for loop, identifier ou command
                                           K KWLoop  -> cpa{cnt = if(bval $ va) then S (C $ L (beval vb) (cmdval vb)) : tail c 
                                                                                else tail c ,val = vs}
                                           K KWRef   -> let loc = (Map.size s) + 1 in 
                                                             cpa{cnt = tail c, sto = Map.insert (Loc loc) (storeValue va) s, val = Vl (Loc loc) : vs, locs = loc : l}
                                           K KWBind  -> cpa{cnt = tail c, val = (Bng (xval vb) (itval va)) : vs} 
                                           K KWDec   -> cpa{cnt = tail c, val = tail v, env = Map.insert (idtval (head v)) (lcval (head v)) e}
                                           K KWBlk   -> cpa{cnt = tail c, val = tail vs, env = enval va, sto = Map.filterWithKey (\k _ -> not (k `elem` (lvals vb))) s, 
                                                            locs = lvls (head vs)}
-- outra possibilidade: Value incluir o tipo env, e empilhar o env no ValueStack
main :: IO ()
main = do
    --s <- readFile "C:\\Users\\rodri\\OneDrive\\Documentos\\compiladores\\program.txt"
    --print s
    --let ast = parseCalc (scanTokens s)
    let ast = Ae $ Sum (N 1) (N 1)
    --let ast = Comm (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1)))
    --let ast = Comm (CSeq (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1))) (Assign (Id "Meu ID Denovo!") (Aexp $ Sum (Num 1) (Num 1))))
    --let ast = Comm (Loop (Eq (Boo False) (Boo False)) (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1))))
    --let ast = Comm (CSeq (Assign (Id "Meu ID") (Aexp $ Num 1)) (Loop (Lt (Id "Meu ID") (Num 5)) (Assign (Id "Meu ID") (Aexp $ Sum (Id "Meu ID") (Num 1)))))
    let aut = CmdPiAut (Map.fromList []) (Map.fromList []) [] [S $ E $ ast] []
    let result = eval aut
    print ast
    print result
{- Example expressions
S $ C $ L (B True) (C (A (I "Meu ID") (Sum (N 1) (N 1))))
-}
