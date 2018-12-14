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
                     (eval $ CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList [(1, Sint 2)]) [] [S $ E $ Be $ Lt (Id $ I "x") (N 4)] [])
                     (CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList[(1, Sint 2)]) [Vb True] [] [])
--  Como usar: carregar testes no ghci com ":l Tests", em seguida "runTestTT expressionTests"

expressionTests = TestList [testNum, testBoo, testSum, testSub, testMul, testEq, testNot, testGt, testGe, testLt, testLe, testAnd, testOr, testIdLt]

-- Command tests

testAssign = TestCase $ assertEqual "Assign command test"
                         (eval $ CmdPiAut (Map.fromList [(I "Meu ID", Lo 1)]) (Map.fromList []) [] [S $ C $ A (I "Meu ID") (Ae (N 5))] [])
                         (CmdPiAut (Map.fromList [(I "Meu ID", Lo 1)]) (Map.fromList[(1, Sint 5)]) [] [] [])

testCmdSeq = TestCase $ assertEqual "Command Sequence test"
                         (eval $ CmdPiAut (Map.fromList [(I "Meu ID", Lo 1), (I "Meu ID Denovo", Lo 2)]) (Map.fromList []) [] 
                                          [S $ C $ Cs (A (I "Meu ID") (Ae (N 5))) (A (I "Meu ID Denovo") (Ae (N 7)))] []) 
                         (CmdPiAut (Map.fromList [(I "Meu ID", Lo 1), (I "Meu ID Denovo", Lo 2)]) (Map.fromList[(1, Sint 5), (2, Sint 7)]) [] [] [])

testLoop = TestCase $ assertEqual "Loop command test" 
                         (eval $ CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList [(1, Sint 1)]) []
                                          [S $ C (L (Lt (Id $ I "x") (N 9)) (A (I "x") (Ae (Sum (Id $ I "x") (N 1)))))] [])
                         (CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList[(1, Sint 9)]) [] [] [])

commandTests = TestList [testAssign, testCmdSeq, testLoop]

testRf = TestCase $ assertEqual "Ref declaration test"
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [(S $ E $ Rf $ Ae $ N 1)] [])
                         (CmdPiAut (Map.fromList []) (Map.fromList [(1, Sint 1)]) [Vbi $ Lo 1] [] [1])

testDr = TestCase $ assertEqual "DeRef declaration test"
                         (eval $ CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList []) [] [(S $ E $ Dr $ I "x")] [])
                         (CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList []) [Vbi $ Lo 1] [] [])

testVr = TestCase $ assertEqual "ValRef declaration test"
                         (eval $ CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList [(1, Sint 2), (2, Sint 1)]) [] [(S $ E $ Vr $ I "x")] [])
                         (CmdPiAut (Map.fromList [(I "x", Lo 1)]) (Map.fromList [(1, Sint 2), (2, Sint 1)]) [Vi 1] [] [])

testBindEnvs = TestCase $ assertEqual "Bind declaration test, with plus envs"
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [Env (Map.fromList [(I "y", Lo 2)])] [(S $ D $ Bi (I "x") (Rf (Ae (N 7))))] [])
                         (CmdPiAut (Map.fromList []) (Map.fromList [(1, Sint 7)]) [Env (Map.fromList [(I "y", Lo 2), (I "x", Lo 1)])] [] [1])

testBindOnly = TestCase $ assertEqual "Bind declaration test, bind mapping only"
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [(S $ D $ Bi (I "x") (Rf (Ae (N 7))))] [])
                         (CmdPiAut (Map.fromList []) (Map.fromList [(1, Sint 7)]) [Env (Map.fromList [(I "x", Lo 1)])] [] [1])

testDec = TestCase $ assertEqual "DecKW test"
                         (CmdPiAut (Map.fromList []) (Map.fromList []) [] [] [])
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [(S $ C $ Bl (Bi (I "x") (Rf (Ae (N 7)))) (A (I "x") (Ae (N 4))))] [])

testBlk = TestCase $ assertEqual "Block declaration test"
                         (CmdPiAut (Map.fromList []) (Map.fromList [(1, Sint 4)]) [] [] [])
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [(S $ C $ Bl (Bi (I "x") (Rf (Ae (N 7)))) (A (I "x") (Ae (N 4))))] [])

declarationsTests = TestList [testRf, testDr, testVr, testBindOnly, testBindEnvs, testBlk]

testAbs = TestCase $ assertEqual "Abstraction test"
                         (CmdPiAut (Map.fromList []) (Map.fromList []) [Vclj $ Clj [I "x"] (A (I "x") (Ae (N 4))) (Map.fromList [])] [] [])
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [Ab $ Abs [I "x"] (A (I "x") (Ae (N 4)))] [])

testBindAbs = TestCase $ assertEqual "Bind Abstraction test"
                         (CmdPiAut (Map.fromList []) (Map.fromList []) ([Env {enval = Map.fromList [(I "x",Cl {cl = Clj {formals = [I "y",I "z"], blk = A (I "x") (Ae (N 4)) , local = Map.fromList []}})]}]) [] [])
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [S $ D $ Bn (I "x") (Abs [I "y", I "z"] (A (I "x") (Ae (N 4))) )] [])

testCall = TestCase $ assertEqual "Call test"
                         (CmdPiAut (Map.fromList []) (Map.fromList [(1, Sint 4)]) [] [] [])
                         (eval $ CmdPiAut (Map.fromList [(I "x",Cl {cl = Clj {formals = [I "y",I "z"], blk = Bl (Bi (I "x") (Rf (Ae (N 7)))) (A (I "x") (Ae (N 4))), local = Map.fromList []}})]) (Map.fromList []) [] [S $ C $ Call (I "x") [(Ae $ N 1), (Ae $ N 2)]] [])

fullTest = TestCase $ assertEqual "full test"
                         (CmdPiAut (Map.fromList []) (Map.fromList []) [] [] [])
                         (eval $ CmdPiAut (Map.fromList []) (Map.fromList []) [] [S $ C (Bl {decl = Bi (I "y") (Rf (Ae (N 0))), kmd = Bl {decl = Bn (I "f") (Abs [I "c",I "b",I "a"] (A (I "y") (Ae (Mul (Mul (Id (I "a")) (Id (I "b"))) (Id (I "c")))))), kmd = Call (I "f") [Ae (N 9),Ae (N 7),Ae (N 1)]}})] []) 

abstractionsTests = TestList[testAbs, testBindAbs, testCall, fullTest]

evalStorableTest1 = TestCase $ assertEqual "eval Storable test" (Vi 4) (evalStorable $ Sint 4)
evalStorableTest2 = TestCase $ assertEqual "eval Storable test 2" (Vb True) (evalStorable $ Sbool True)
                        
storeValueTest1 = TestCase $ assertEqual "store Value test" (Sint 4) (storeValue $ Vi 4)

expressBindableTest1 = TestCase $ assertEqual "express bindable test" (Ae (N 4)) (expressBindable $ Sto (Sint 4))

matchTest = TestCase $ assertEqual "match test" (Map.fromList [(I "x", Sto $ Sbool True), (I "y", Sto $ Sint 4)]) (match [I "x", I "y"] [Vb True, Vi 4])

buildDeclTest = TestCase $ assertEqual "build decl test" (Ds (Bi (I "x") (Ae (N 4))) None) (buildDecl [(I "x", Sto $ Sint 4)])
buildDeclTest2 = TestCase $ assertEqual "build decl test 2" (Ds (Bi (I "x") (Ae (N 4))) (Ds (Bi (I "y") (Ae (N 2))) None)) (buildDecl [(I "x", Sto $ Sint 4), (I "y", Sto $ Sint 2)])

subTests = TestList[evalStorableTest1, evalStorableTest2, storeValueTest1, expressBindableTest1, matchTest, buildDeclTest, buildDeclTest2]
