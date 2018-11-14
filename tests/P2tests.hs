module Main where
import Tests
import System.IO
import qualified Data.Map.Strict as Map
import Test.HUnit

main :: IO ()
main = do
    print "Declarations tests"
    cmd <- runTestTT declarationsTests
    print ""
