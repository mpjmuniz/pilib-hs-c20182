module Main where
import Grammar
import Tokens
import Data.Dynamic

type Env = String -> Exp
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

--run :: Exp -> Int
--run e = eval e emptyEnv

main :: IO ()
main = do
    s <- getLine
    print s
    let ast = parseCalc (scanTokens s)
    --ExpPiAut [][ast]
    --ExpPiAut = ExpPiAut v ast
    print (dynTypeRep (toDyn ast))
    --eval()
