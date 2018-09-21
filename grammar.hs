{-# LANGUAGE RecordWildCards #-}

import Data.Typeable
import Data.Maybe

{- |
 - Pi-lib expressions. 
 -
 - These will be used futurely by the parser, and will be returned according to the tokens found in the imp program.
-}
data Identifier = Id String deriving (Show, Eq)
data Expr = Aexp Aexpr | Bexp Bexpr | Idtf Identifier | Kw Keyword | Comm Cmd deriving (Show, Eq)
data Aexpr = Num Int | Sum Aexpr Aexpr | Sub Aexpr Aexpr | Mul Aexpr Aexpr deriving (Show, Eq)
data Bexpr = Boo Bool | Eq Bexpr Bexpr | Not Bexpr | Gt Bexpr Bexpr | Ge Bexpr Bexpr 
           | Lt Bexpr Bexpr | Le Bexpr Bexpr | And Bexpr Bexpr | Or Bexpr Bexpr deriving (Show, Eq)
data Statement = Exp Expr | Command Cmd deriving Show
data Cmd = Assign Identifier Expr | Loop Bexpr Cmd | CSeq Cmd Cmd deriving (Show, Eq)
data Keyword = KWSum | KWMul | KWSub 
             | KWEq | KWNot | KWOr | KWAnd | KWLt | KWLe | KWGt | KWGe 
             | KWAssign | KWLoop deriving (Show, Eq)
data Value = Bo { bval :: Bool } | In { ival :: Int } | Idt { idval :: Identifier } 
           | Lp {beval :: Bexpr, cmdval :: Cmd} | Comd {cval :: Cmd } deriving (Show, Eq)
{- |
 - Automata definition
 -}
type ValueStack = [Value]
type ControlStack = [Expr]
{-data cpa= cpa{ val :: ValueStack,
                           cnt :: ControlStack
                         } deriving Show-}

type Loc = Integer
type Env = [(Value,Expr)]
type Sto = [(Expr,Value)]
data CmdPiAut = CmdPiAut { env :: Env,
                           sto :: Sto,
                           loc :: Loc,
                           val :: ValueStack,
                           cnt :: ControlStack
                         }
-- TODO: rewrite fromJust lookup key map to only a lookup as composition of both fns
eval :: CmdPiAut -> CmdPiAut
eval cpa@(CmdPiAut e s l v c) = case (head c) of
                                 Comm (Assign idtf exp) -> eval cpa{cnt = exp : Kw KWAssign : tail c, val = Idt idtf : v} 
                                 Comm (CSeq cmd1 cmd2)  -> eval cpa{cnt = Comm cmd1 : Comm cmd2 : tail c}
                                 Comm (Loop bexp cmd)   -> eval cpa{cnt = Bexp bexp : Kw KWLoop : tail c, val = Comd cmd : Lp bexp cmd : v}
                                 Aexp (Sum aex1 aex2)   -> eval cpa{cnt = Aexp aex1 : Aexp aex2 : Kw KWSum : tail c, val = v} 
                                 Aexp (Sub aex1 aex2)   -> eval cpa{cnt = Aexp aex1 : Aexp aex2 : Kw KWSub : tail c, val = v}
                                 Aexp (Mul aex1 aex2)   -> eval cpa{cnt = Aexp aex1 : Aexp aex2 : Kw KWMul : tail c, val = v}
                                 Bexp (Eq  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWEq  : tail c, val = v} 
                                 Bexp (Or  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWOr  : tail c, val = v} 
                                 Bexp (And exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWAnd  : tail c, val = v} 
                                 Bexp (Lt  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWLt  : tail c, val = v} 
                                 Bexp (Le  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWLe  : tail c, val = v} 
                                 Bexp (Ge  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWGe  : tail c, val = v} 
                                 Bexp (Gt  exp1 exp2)   -> eval cpa{cnt = Bexp exp1 : Bexp exp2 : Kw KWGt  : tail c, val = v} 
                                 Bexp (Not ex)          -> eval cpa{cnt = Bexp ex : Kw KWNot : tail c, val = v}
                                 Aexp (Num ival)        -> eval cpa{cnt = tail c, val = In ival : v}
                                 Bexp (Boo bval)        -> eval cpa{cnt = tail c, val = Bo bval : v}
                                 Idtf (Id str)          -> eval cpa{cnt = tail c, val = fromJust (lookup (fromJust (lookup (Idt (Id str)) e)) s) : v }
                                 Kw KWAssign            -> eval cpa{cnt = tail c, val = tail (tail v), sto = (fromJust (lookup (head (tail v)) e), head v) : s }
                                 --Kw KWLoop            -> eval cpa{val = tail v, if beval (head v) of }
                                 Kw KWSum               -> eval cpa{cnt = tail c, val = In (ival (head v) + ival (head (tail v))) : tail (tail v)}
                                 Kw KWSub               -> eval cpa{cnt = tail c, val = In (ival (head v) - ival (head (tail v))) : tail (tail v)}
                                 Kw KWMul               -> eval cpa{cnt = tail c, val = In (ival (head v) * ival (head (tail v))) : tail (tail v)}
                                 Kw KWEq                -> eval cpa{cnt = tail c, val = Bo (bval (head v) == bval (head (tail v))) : tail (tail v)}
                                 Kw KWLe                -> eval cpa{cnt = tail c, val = Bo (bval (head v) <= bval (head (tail v))) : tail (tail v)}
                                 Kw KWLt                -> eval cpa{cnt = tail c, val = Bo (bval (head v) < bval (head (tail v))) : tail (tail v)}
                                 Kw KWGe                -> eval cpa{cnt = tail c, val = Bo (bval (head v) >= bval (head (tail v))) : tail (tail v)}
                                 Kw KWGt                -> eval cpa{cnt = tail c, val = Bo (bval (head v) > bval (head (tail v))) : tail (tail v)}
                                 Kw KWOr                -> eval cpa{cnt = tail c, val = Bo (bval (head v) || bval (head (tail v))) : tail (tail v)}
                                 Kw KWAnd               -> eval cpa{cnt = tail c, val = Bo (bval (head v) && bval (head (tail v))) : tail (tail v)}
                                 Kw KWNot               -> eval cpa{cnt = tail c, val = Bo (not (bval (head v))) : tail v}
{- Example expressions
cpa[] [Sum (Num 5) (Num 2)]

-}
