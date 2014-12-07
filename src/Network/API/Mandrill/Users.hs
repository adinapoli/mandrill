
module Network.API.Mandrill.Users where

import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.HTTP
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Users.Types
import           Network.HTTP.Client


--------------------------------------------------------------------------------
-- | Return the information about the API-connected user
info :: MandrillKey -> Maybe Manager -> IO (MandrillResponse UsersInfoResponse)
info key = toMandrillResponse UsersInfo (UsersRq key)


--------------------------------------------------------------------------------
-- | Validate an API key and respond to a ping
ping :: MandrillKey -> Maybe Manager -> IO (MandrillResponse UsersPingResponse)
ping _ _ = fail "users/ping.json doesn't return valid JSON, thus is not implemented yet."


--------------------------------------------------------------------------------
-- | Validate an API key and respond to a ping (anal JSON parser version)
ping2 :: MandrillKey -> Maybe Manager -> IO (MandrillResponse UsersPing2Response)
ping2 key = toMandrillResponse UsersPing2 (UsersRq key)


--------------------------------------------------------------------------------
-- | Return the senders that have tried to use this account, both verified and unverified
senders :: MandrillKey -> Maybe Manager -> IO (MandrillResponse [UsersSendersResponse])
senders key = toMandrillResponse UsersSenders (UsersRq key)
