module Tests where
import Test.HUnit
import qualified Data.Map.Strict as Map
import Calc
import Parser

-- CmdPiAut aplicado em menos argumentos que o esperado o transforma num novo construtor que pode ser reutilizado com qualquer argumento
baseAut = CmdPiAut (Map.fromList []) (Map.fromList [])

testNum = TestCase $ assertEqual "Number expression test" 
                     (eval $ baseAut [] [S $ E $ Ae $ N 2] []) 
                     (baseAut [Vi 2] [] [])

testBoo = TestCase $ assertEqual "Boolean expression test"
                     (eval $ baseAut [] [S $ E $ Be $ B True] [])
                     (baseAut [Vb True] [] [])

testSum = TestCase $ assertEqual "Sum expression test"
                     (eval $ baseAut [] [S $ E $ Ae $ Sum (N 1) (N 1)] [])
                     (baseAut [Vi 2] [] [])

testSub = TestCase $ assertEqual "Sub expression test"
                     (eval $ baseAut [] [S $ E $ Ae $ Sub (N 1) (N 1)] [])
                     (baseAut [Vi 0] [] [])

testMul = TestCase $ assertEqual "Mul expression test"
                     (eval $ baseAut [] [S $ E $ Ae $ Mul (N 1) (N 1)] [])
                     (baseAut [Vi 1] [] [])

testEq = TestCase $ assertEqual "Eq expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Eq (B True) (B False)] [])
                     (baseAut [Vb False] [] [])

testNot = TestCase $ assertEqual "Not expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Not (B True)] [])
                     (baseAut [Vb False] [] [])

testGt = TestCase $ assertEqual "Gt expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Gt (N 2) (N 1)] [])
                     (baseAut [Vb True] [] [])

testGe = TestCase $ assertEqual "Ge expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Ge (N 2) (N 2)] [])
                     (baseAut [Vb True] [] [])

testLt = TestCase $ assertEqual "Lt expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Lt (N 1) (N 2)] [])
                     (baseAut [Vb True] [] [])

testLe = TestCase $ assertEqual "Le expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Le (N 2) (N 2)] [])
                     (baseAut [Vb True] [] [])

testAnd = TestCase $ assertEqual "And expression test"
                     (eval $ baseAut [] [S $ E $ Be $ And (B True) (B True)] [])
                     (baseAut [Vb True] [] [])

testOr = TestCase $ assertEqual "Or expression test"
                     (eval $ baseAut [] [S $ E $ Be $ Or (B True) (B False)] [])
                     (baseAut [Vb True] [] [])

testIdLt = TestCase $ assertEqual "Lt expression with Id test"
                     (eval $ CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList [(Loc 1, Right 2)]) [] [S $ E $ Be $ Lt (Id $ I "x") (N 4)] [])
                     (CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList[(Loc 1, Right 2)]) [Vb True] [] [])
--  Como usar: carregar testes no ghci com ":l Tests", em seguida "runTestTT expressionTests"

expressionTests = TestList [testNum, testBoo, testSum, testSub, testMul, testEq, testNot, testGt, testGe, testLt, testLe, testAnd, testOr, testIdLt]

-- Command tests

testAssign = TestCase $ assertEqual "Assign command test"
                         (eval $ CmdPiAut (Map.fromList [(I "Meu ID", Loc 1)]) (Map.fromList []) [] [S $ C $ A (I "Meu ID") (Ae (N 5))] [])
                         (CmdPiAut (Map.fromList [(I "Meu ID", Loc 1)]) (Map.fromList[(Loc 1, Right 5)]) [] [] [])

testCmdSeq = TestCase $ assertEqual "Command Sequence test"
                         (eval $ CmdPiAut (Map.fromList [(I "Meu ID", Loc 1), (I "Meu ID Denovo", Loc 2)]) (Map.fromList []) [] 
                                          [S $ C $ Cs (A (I "Meu ID") (Ae (N 5))) (A (I "Meu ID Denovo") (Ae (N 7)))] []) 
                         (CmdPiAut (Map.fromList [(I "Meu ID", Loc 1), (I "Meu ID Denovo", Loc 2)]) (Map.fromList[(Loc 1, Right 5), (Loc 2, Right 7)]) [] [] [])

testLoop = TestCase $ assertEqual "Loop command test" 
                         (eval $ CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList [(Loc 1, Right 1)]) []
                                          [S $ C (L (Lt (Id $ I "x") (N 9)) (A (I "x") (Ae (Sum (Id $ I "x") (N 1)))))] [])
                         (CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList[(Loc 1, Right 9)]) [] [] [])

commandTests = TestList [testAssign, testCmdSeq, testLoop]

testRf = TestCase $ assertEqual "Ref declaration test"
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [(S $ E $ Rf $ Ae $ N 1)] [])
                         (CmdPiAut (Map.fromList []) (Map.fromList [(Loc 1, Right 1)]) [Vl $ Loc 1] [] [1])

testDr = TestCase $ assertEqual "DeRef declaration test"
                         (eval $ CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList []) [] [(S $ E $ Dr $ I "x")] [])
                         (CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList []) [Vl $ Loc 1] [] [])

testVr = TestCase $ assertEqual "ValRef declaration test"
                         (eval $ CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList [(Loc 1, Right 2), (Loc 2, Right 1)]) [] [(S $ E $ Vr $ I "x")] [])
                         (CmdPiAut (Map.fromList [(I "x", Loc 1)]) (Map.fromList [(Loc 1, Right 2), (Loc 2, Right 1)]) [Vi 1] [] [])

declarationsTests = TestList [testRf, testDr, testVr]

-- Declaration tests
-- testes restantes
{-
testDecSeq
testBind
testBlock
testVr
testDr
-}
