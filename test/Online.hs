{-# LANGUAGE OverloadedStrings #-}
module Online where

import           Data.Aeson
import qualified Data.ByteString.Char8               as C8
import           Data.Either
import           Network.API.Mandrill.Messages.Types
import           Network.API.Mandrill.Users.Types
import           RawData
import           Test.QuickCheck
import           Test.Tasty.HUnit
import           Text.RawString.QQ

import qualified Network.API.Mandrill.Inbound        as API
import qualified Network.API.Mandrill.Messages       as API
import           Network.API.Mandrill.Types
import qualified Network.API.Mandrill.Users          as API

--
-- Users calls
--
testOnlineUsersInfo :: MandrillKey -> Assertion
testOnlineUsersInfo k = do
  res <- API.info k Nothing
  case res of
    MandrillSuccess _ -> return ()
    MandrillFailure e -> fail $ "users/info.json " ++ show e

testOnlineUsersPing2 :: MandrillKey -> Assertion
testOnlineUsersPing2 k = do
  res <- API.ping2 k Nothing
  case res of
    MandrillSuccess _ -> return ()
    MandrillFailure e -> fail $ "users/ping2.json " ++ show e

testOnlineUsersSenders :: MandrillKey -> Assertion
testOnlineUsersSenders k = do
  res <- API.senders k Nothing
  case res of
    MandrillSuccess _ -> return ()
    MandrillFailure e -> fail $ "users/senders.json " ++ show e


--
-- Messages calls
--
testOnlineMessagesSend :: MandrillKey -> Assertion
testOnlineMessagesSend k = do
  msg <- generate arbitrary
  res <- API.send k msg Nothing Nothing Nothing Nothing
  case res of
    MandrillSuccess _ -> return ()
    MandrillFailure e -> fail $ "messages/send.json " ++ show e

--
-- Inbound calls
--
testOnlineDomainAdd :: MandrillKey -> Assertion
testOnlineDomainAdd k = do
  res <- API.addDomain k "foobar.com" Nothing
  case res of
   MandrillSuccess a -> print a
   MandrillFailure e -> fail $ "inbound/add-domain.json " ++ show e

testOnlineRouteAdd :: MandrillKey -> Assertion
testOnlineRouteAdd k = do
  res <- API.addRoute k "foobar.com" "mail-*" "http://requestb.in/19a4frw1"  Nothing
  case res of
   MandrillSuccess a -> print a
   MandrillFailure e -> fail $ "inbound/add-domain.json " ++ show e
