module Main where
import Calc
import Parser
import Lexer
import System.IO
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    print "Welcome to the imp automaton. Write some imp line to be evaluated:"
    s <- getLine
    print s
    let ast = parseCalc (scanTokens s)
  -- let ast = Ae $ Sum (N 1) (N 1)
    --let ast = Comm (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1)))
    --let ast = Comm (CSeq (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1))) (Assign (Id "Meu ID Denovo!") (Aexp $ Sum (Num 1) (Num 1))))
    --let ast = Comm (Loop (Eq (Boo False) (Boo False)) (Assign (Id "Meu ID") (Aexp $ Sum (Num 1) (Num 1))))
    --let ast = Comm (CSeq (Assign (Id "Meu ID") (Aexp $ Num 1)) (Loop (Lt (Id "Meu ID") (Num 5)) (Assign (Id "Meu ID") (Aexp $ Sum (Id "Meu ID") (Num 1)))))
    let aut = CmdPiAut (Map.fromList []) (Map.fromList []) [] [S $ ast] []
    let result = eval aut
    print ast
    print result
