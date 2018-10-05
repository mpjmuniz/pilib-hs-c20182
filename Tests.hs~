module Tests where
import Test.HUnit
import qualified Data.Map as Map

sumAST = Aexp $ Sum (Num 1) (Num 1)
cmdPiAutSum = CmdPiAut (Map.fromList []) (Map.fromList []) [] [sumAST]
test1 = TestCase (assertEqual "testing sum" (eval cmdPiAutSum) (CmdPiAut {env = fromList [], sto = fromList [], val = [In {ival = 2}], cnt = []}))

commandAutomatonTests = TestList [TestCase test1]
