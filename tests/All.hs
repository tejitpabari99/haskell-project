import Test.Tasty
import Test.Tasty.HUnit
import Control.Applicative ((<$>))

import qualified Backgammon.ModelTests as Model
import qualified Backgammon.FormatTests as Format

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [ Model.unitTests
                                   , Format.unitTests
                                   ]
