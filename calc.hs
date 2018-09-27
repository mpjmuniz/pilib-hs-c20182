module Main where
import Grammar
import Tokens
import Data.Dynamic
import System.IO
import qualified Data.Map.Strict as Map


{- |
 - Automata definition
 -}
type ValueStack = [Value]
type ControlStack = [Expr]
type Loc = Integer
type Env = [(Value,Expr)]
type Sto = [(Expr,Value)]
data CmdPiAut = CmdPiAut { env :: Map.Map Identifier Location,
                           sto :: Map.Map Location Value, -- TODO: mudar para Storable	
                           val :: ValueStack,
                           cnt :: ControlStack
                         } deriving Show

--lookup' :: Ord k => k -> Map.Map k a -> a
--lookup' k m = fromJust $ Map.lookup k m		

				 
eval :: CmdPiAut -> CmdPiAut
eval cpa@(CmdPiAut {cnt = []})= cpa
eval cpa@(CmdPiAut e s v c)    = eval $ case (head c) of
                                 Comm (Assign idtf exp) -> eval cpa{cnt = exp : Kw KWAssign : tail c, val = Idt idtf : v} 
                                 Comm (CSeq cmd1 cmd2)  -> eval cpa{cnt = Comm cmd1 : Comm cmd2 : tail c}
                                 Comm (Loop bexp cmd)   -> eval cpa{cnt = Bexp bexp : Kw KWLoop : tail c, val = Comd cmd : Lp bexp cmd : v}
                                 Aexp (Sum aex1 aex2)   -> eval cpa{cnt = Aexp aex1 : Aexp aex2 : Kw KWSum : tail c} 
                                 Aexp (Sub aex1 aex2)   -> eval cpa{cnt = Aexp aex2 : Aexp aex1 : Kw KWSub : tail c}
                                 Aexp (Mul aex1 aex2)   -> eval cpa{cnt = Aexp aex1 : Aexp aex2 : Kw KWMul : tail c}
                                 Bexp (Eq  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWEq  : tail c} 
                                 Bexp (Or  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWOr  : tail c} 
                                 Bexp (And exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp1 : Kw KWAnd  : tail c} 
                                 Bexp (Lt  exp1 exp2)   -> eval cpa{cnt = Aexp exp2 : Aexp exp1 : Kw KWLt  : tail c} 
                                 Bexp (Le  exp1 exp2)   -> eval cpa{cnt = Aexp exp2 : Aexp exp1 : Kw KWLe  : tail c} 
                                 Bexp (Ge  exp1 exp2)   -> eval cpa{cnt = Aexp exp2 : Aexp exp1 : Kw KWGe  : tail c} 
                                 Bexp (Gt  exp1 exp2)   -> eval cpa{cnt = Aexp exp2 : Aexp exp1 : Kw KWGt  : tail c} 
                                 Bexp (Not ex)          -> eval cpa{cnt = Bexp ex : Kw KWNot : tail c, val = v}
                                 Aexp (Num ival)        -> eval cpa{cnt = tail c, val = In ival : v}
                                 Bexp (Boo bval)        -> eval cpa{cnt = tail c, val = Bo bval : v}
                                 Kw KWNot               -> cpa{cnt = tail c, val = Bo (not (bval (head v))) : tail v}
                                 x -> let (va:vb:vs) = v in
                                                            case x of 
                                                            --Kw KWAssign -> cpa{cnt = tail c, val = vs, sto = Map.insert (lookup' (idval vb) e) va s }
                                                            Kw KWLoop -> cpa{cnt = if(bval $ head v) 
                                                                                   then (Comm $ Loop (beval $ head $ tail v) (cmdval $ head $ tail v)) : tail c 
                                                                                   else tail c
                                                                            , val = tail $ tail v}
                                                            Kw KWSum -> cpa{cnt = tail c, val = In (ival (head v) + ival (head (tail v))) : tail (tail v)}
                                                            Kw KWSub -> cpa{cnt = tail c, val = In (ival (head v) - ival (head (tail v))) : tail (tail v)}
                                                            Kw KWMul -> cpa{cnt = tail c, val = In (ival (head v) * ival (head (tail v))) : tail (tail v)}
                                                            Kw KWEq  -> cpa{cnt = tail c, val = Bo (bval (head v) == bval (head (tail v))) : tail (tail v)}
                                                            Kw KWLe  -> cpa{cnt = tail c, val = Bo (ival (head v) >= ival (head (tail v))) : tail (tail v)}
                                                            Kw KWLt  -> cpa{cnt = tail c, val = Bo (ival (head v) > ival (head (tail v))) : tail (tail v)}
                                                            Kw KWGe  -> cpa{cnt = tail c, val = Bo (ival (head v) <= ival (head (tail v))) : tail (tail v)}
                                                            Kw KWGt  -> cpa{cnt = tail c, val = Bo (ival (head v) < ival (head (tail v))) : tail (tail v)}
                                                            Kw KWOr  -> cpa{cnt = tail c, val = Bo (bval (head v) || bval (head (tail v))) : tail (tail v)}
                                                            Kw KWAnd -> cpa{cnt = tail c, val = Bo (bval (head v) && bval (head (tail v))) : tail (tail v)}
main :: IO ()
main = do
    s <- readFile "C:\\Users\\rodri\\OneDrive\\Documentos\\compiladores\\program.txt"
    print s
    let ast = parseCalc (scanTokens s)
    --let aut = CmdPiAut [] [] [] [ast]
    --let result =  eval aut
    print ast
    --print result
