{-# LANGUAGE CPP #-}
module Tests where

import Test.Tasty.HUnit
import Data.Either
import Data.Aeson
import Network.API.Mandrill.Messages.Types
import Network.API.Mandrill.Users.Types
import qualified Data.ByteString.Char8 as C8
import RawData

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 706
isRight :: Either a b -> Bool
isRight (Left _) = False
isRight _ = True
#endif

testMessagesSend :: Assertion
testMessagesSend = 
  assertBool ("send.json: Parsing failed! " ++ show parsePayload)
             (isRight parsePayload)
  where
    parsePayload :: Either String MessagesSendRq
    parsePayload = eitherDecodeStrict . C8.pack $ sendData

testUsersInfo :: Assertion
testUsersInfo = do
  let res = parsePayload
  assertBool ("users/info.json (response): Parsing failed: " ++ show res)
             (isRight parsePayload)
  where
    parsePayload :: Either String UsersInfoResponse
    parsePayload = eitherDecodeStrict . C8.pack $ usersInfoData

testUsersSenders :: Assertion
testUsersSenders = do
  let res = parsePayload
  assertBool ("users/senders.json (response): Parsing failed: " ++ show res)
             (isRight parsePayload)
  where
    parsePayload :: Either String UsersSendersResponse
    parsePayload = eitherDecodeStrict . C8.pack $ usersSendersData
