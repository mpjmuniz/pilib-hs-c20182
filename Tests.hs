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
--  Como usar: carregar testes no ghci com ":l Tests", em seguida "runTestTT expressionTests"

expressionTests = TestList [testNum, testBoo, testSum, testSub, testMul, testEq, testNot, testGt, testGe, testLt, testLe, testAnd, testOr]
