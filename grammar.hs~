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

{- |
 - Automata definition
 -}
type ValueStack = [String]
type ControlStack = [String]
data ExpPiAut = ExpPiAut { val :: ValueStack,
                           cnt :: ControlStack
                         } deriving Show
{- |
 - automata functions evaluation
-}
evalSum :: ExpPiAut -> Aexpr -> Aexpr -> ExpPiAut
evalSum (ExpPiAut v c) aex1 aex2 = ExpPiAut {cnt = (show aex2 : show aex1 : "SUM" : c ), val = v}

-- TODO: implement eval
eval :: ExpPiAut -> ExpPiAut
eval (ExpPiAut v c) = if typeOf (head c) true (evalSum ExpPiAut {cnt = rest} (head ) (head v)

{- 
data ExpKW = SUM | SUB | MUL | EQ | NOT
evalSum aut exp exp = aut&cnt 
aut@(ExpPiAut { cnt = cnts@(ControlStack) }) = aut {cnt = cnt ++ exp1 ++ exp2}
-}
