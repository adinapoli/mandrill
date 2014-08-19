{-# LANGUAGE OverloadedStrings #-}
module Online where

import           Test.QuickCheck
import           Test.Tasty.HUnit
import           Text.RawString.QQ
import           Data.Either
import           Data.Aeson
import           Network.API.Mandrill.Messages.Types
import           Network.API.Mandrill.Users.Types
import qualified Data.ByteString.Char8 as C8
import           RawData

import           Network.API.Mandrill.Types
import qualified Network.API.Mandrill.Messages as API


testOnlineMessagesSend :: MandrillKey -> Assertion
testOnlineMessagesSend k = do
  msg <- generate arbitrary
  res <- API.send k msg Nothing Nothing Nothing
  case res of
    MandrillSuccess _ -> return ()
    MandrillFailure e -> fail $ "messages/send.json " ++ show e
