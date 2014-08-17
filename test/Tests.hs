module Tests where

import Test.QuickCheck
import Test.Tasty.HUnit
import Text.RawString.QQ
import Data.Either
import Data.Aeson
import Network.API.Mandrill.Messages.Types
import Network.API.Mandrill.Users.Types
import qualified Data.ByteString.Char8 as C8
import RawData

testMessagesSend :: Assertion
testMessagesSend = assertBool "send.json: Parsing failed!" (isRight parsePayload)
  where
    parsePayload :: Either String MessagesSendRq
    parsePayload = eitherDecodeStrict . C8.pack $ sendData

testUsersInfo :: Assertion
testUsersInfo = do
  let res = parsePayload
  assertBool ("users.json (response): Parsing failed: " ++ show res)
             (isRight parsePayload)
  where
    parsePayload :: Either String UsersResponse
    parsePayload = eitherDecodeStrict . C8.pack $ usersInfoData
