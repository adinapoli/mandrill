{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid
import qualified Data.Text             as T
import           Online
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Tests

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
                   testCase "users/info.json"        (testOnlineUsersInfo k)
                 , testCase "users/ping2.json"       (testOnlineUsersPing2 k)
                 , testCase "users/senders.json"     (testOnlineUsersSenders k)
                 , testCase "messages/send.json"     (testOnlineMessagesSend k)
                 , testCase "inbound/addDomain.json" (testOnlineDomainAdd k)
                 , testCase "inbound/addRoute.json"  (testOnlineRouteAdd k)
                 ]]

----------------------------------------------------------------------
main :: IO ()
main = do
  onlineTests <- onlineTestsIfEnabled
  defaultMainWithIngredients defaultIngredients $
    testGroup "Mandrill tests" $ onlineTests <> [
         testGroup "Mandrill offline tests" [
           testCase "users/info.json API parsing"    testUsersInfo
         , testCase "users/senders.json API parsing" testUsersSenders
         , testCase "messages/send.json API parsing" testMessagesSend
         , testCase "messages/send.json API response parsing" testMessagesResponseRejected
         , testCase "inbound/add-route.json API response parsing" testRouteAdd
         , testCase "inbound/add-domain.json API response parsing" testDomainAdd
         , testCase "senders/verify-domain.json API response parsing" testVerifyDomain
         ]
     ]
