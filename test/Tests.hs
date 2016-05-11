{-# LANGUAGE CPP #-}
module Tests where

import           Data.Aeson
import qualified Data.ByteString.Char8               as C8
import           Data.Either
import           Network.API.Mandrill.Inbound
import           Network.API.Mandrill.Messages.Types
import           Network.API.Mandrill.Senders
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Users.Types
import           RawData
import           Test.Tasty.HUnit

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

testMessagesResponseRejected :: Assertion
testMessagesResponseRejected = do
  assertBool ("send.json response: Parsing failed" ++ show parsePayload)
             (isRight parsePayload)
  where
    parsePayload :: Either String (MandrillResponse [MessagesResponse])
    parsePayload = eitherDecodeStrict . C8.pack $ messagesResponseRejected

testDomainAdd :: Assertion
testDomainAdd =
  assertBool ("inbound/add-domain.json (response): parsing failed" ++ show parsePayload)
             (isRight parsePayload)
  where
    parsePayload :: Either String DomainAddResponse
    parsePayload = eitherDecodeStrict . C8.pack $ domainAdd


testRouteAdd :: Assertion
testRouteAdd =
  assertBool ("inbound/add-route.json (response): parsing failed" ++ show parsePayload)
             (isRight parsePayload)
  where
    parsePayload :: Either String RouteAddResponse
    parsePayload = eitherDecodeStrict . C8.pack $ routeAdd

testVerifyDomain :: Assertion
testVerifyDomain =
  assertBool ("senders/verify-domain.json" ++ show parsePayload)
             (isRight parsePayload)
  where
    parsePayload ::  Either String VerifyDomainResponse
    parsePayload = eitherDecodeStrict . C8.pack $ domainVerify
