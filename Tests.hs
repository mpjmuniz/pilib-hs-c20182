module Tests where
import Test.HUnit
import qualified Data.Map.Strict as Map
import Calc

test1 = TestCase $ assertEqual "test upCase" "FOO" "FOO"
