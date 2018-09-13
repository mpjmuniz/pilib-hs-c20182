{-# LANGUAGE RecordWildCards #-}

import Data.Typeable

{- |
 - Pi-lib expressions. 
 -
 - These will be used futurely by the parser, and will be returned according to the tokens found in the imp program.
-}
data Identifier = Id String deriving Show
data Expr = Aexp Aexpr | Bexp Bexpr | Idtf Identifier deriving Show
data Aexpr = Num Int | Sum Aexpr Aexpr | Sub Aexpr Aexpr | Mul Aexpr Aexpr deriving Show
data Bexpr = Boo Bool | Eq Expr Expr | Not Bexpr deriving Show
data Statement = Exp Expr | Command Cmd deriving Show
data Cmd = Assign Identifier Expr | Loop Bexpr Cmd | CSeq Cmd Cmd deriving Show

data Value = Bo Bool | In Int deriving Show
{- |
 - Automata definition
 -}
type ValueStack = [Value]
type ControlStack = [Expr]
data ExpPiAut = ExpPiAut { val :: ValueStack,
                           cnt :: ControlStack
                         } deriving Show
{- |
 - automata functions evaluation
-}
{-
evalSum :: ExpPiAut -> Aexpr -> Aexpr -> ExpPiAut
evalSum (ExpPiAut v c) aex1 aex2 = ExpPiAut {cnt = ("SUM" : show aex2 : show aex1 : c ), val = v}
-}
-- TODO: implement eval
eval :: ExpPiAut -> ExpPiAut
eval (ExpPiAut v c) = case (head c) of
                           Aexp (Sum aex1 aex2) -> eval ExpPiAut {cnt = Aexp aex1 : Aexp aex2 : c, val = v} -- TODO: discover if KWs are needed here, and why
                           Aexp (Sub aex1 aex2) -> eval ExpPiAut {cnt = Aexp aex1 : Aexp aex2 : c, val = v}
                           Aexp (Mul aex1 aex2) -> eval ExpPiAut {cnt = Aexp aex1 : Aexp aex2 : c, val = v}
                           Bexp (Eq  exp1 exp2) -> eval ExpPiAut {cnt = Bexp exp1 : Bexp exp2 : c, val = v} -- TODO: match types
                           Aexp (Num ival)      -> eval ExpPiAut {cnt = tail c, val = In ival : v}
                           Bexp (Boo bval)      -> eval ExpPiAut {cnt = tail c, val = Bo bval : v}
                           Bexp (Not ex)        -> eval ExpPiAut {cnt = Bexp ex : c, val = v}
{- 
data ExpKW = SUM | SUB | MUL | EQ | NOT
evalSum aut exp exp = aut&cnt 
aut@(ExpPiAut { cnt = cnts@(ControlStack) }) = aut {cnt = cnt ++ exp1 ++ exp2}
-}
