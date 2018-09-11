{-# LANGUAGE RecordWildCards #-}

data Identifier = String deriving Show
data Aexpr = Num Int | Sum Aexpr Aexpr | Sub Aexpr Aexpr | Mul Aexpr Aexpr deriving Show
data Bexpr = Eq Bexpr Bexpr | Not Bexpr deriving Show
data Exp = Aexpr | Bexpr | Id Identifier deriving Show
data Cmd = Assign Identifier Exp | Loop Bexpr Cmd | CSeq Cmd Cmd deriving Show
data Statement = Expr Exp | Command Cmd deriving Show

type ValueStack = [String]
type ControlStack = [String]
data ExpKW = SUM | SUB | MUL | EQ | NOT

data ExpPiAut = ExpPiAut { val :: ValueStack,
                           cnt :: ControlStack
                         } deriving Show

evalSum :: ExpPiAut -> Aexpr -> Aexpr -> ExpPiAut
evalSum aut exp exp = aut&cnt 
aut@(ExpPiAut { cnt = cnts@(ControlStack) }) = aut {cnt = cnt ++ exp1 ++ exp2}