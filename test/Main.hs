{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           Data.Monoid
import           Tests
import           Online
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import qualified Data.Text as T

----------------------------------------------------------------------
withQuickCheckDepth :: TestName -> Int -> [TestTree] -> TestTree
withQuickCheckDepth tn depth tests =
  localOption (QuickCheckTests depth) (testGroup tn tests)

----------------------------------------------------------------------
onlineTestsIfEnabled :: IO [TestTree]
onlineTestsIfEnabled = do
  apiKey <- (return . fmap T.pack) =<< lookupEnv "MANDRILL_API_KEY"
  case apiKey of
    Nothing -> return []
    Just k  -> return [
                 testGroup "Mandrill online tests" [
                   testCase "messages/send.json" (testOnlineMessagesSend k)
                 ]]

----------------------------------------------------------------------
main :: IO ()
main = do
  onlineTests <- onlineTestsIfEnabled
  defaultMainWithIngredients defaultIngredients $
    testGroup "Mandrill tests" $ onlineTests <> [
         testGroup "Mandrill offline tests" [
          testCase "users/info.json API parsing"    testUsersInfo
        , testCase "messages/send.json API parsing" testMessagesSend
        ]
      ]
