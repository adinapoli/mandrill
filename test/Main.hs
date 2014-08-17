
module Main where

import Tests
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

----------------------------------------------------------------------
withQuickCheckDepth :: TestName -> Int -> [TestTree] -> TestTree
withQuickCheckDepth tn depth tests =
  localOption (QuickCheckTests depth) (testGroup tn tests)

----------------------------------------------------------------------
main :: IO ()
main = defaultMainWithIngredients defaultIngredients $
  testGroup "Mandrill tests" [
        testCase "messages/send.json API parsing" testMessagesSend
    ]
