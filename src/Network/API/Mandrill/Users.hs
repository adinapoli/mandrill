
module Network.API.Mandrill.Users where

import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.HTTP
import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Users.Types
import           Data.Aeson


--------------------------------------------------------------------------------
-- | Return the information about the API-connected user
info :: MandrillKey -> IO (MandrillResponse UsersResponse)
info key = toMandrillResponse UsersInfo (UsersRq key)


--------------------------------------------------------------------------------
-- | Validate an API key and respond to a ping
ping :: MandrillKey -> IO (MandrillResponse UsersResponse)
ping key = toMandrillResponse UsersPing (UsersRq key)


--------------------------------------------------------------------------------
-- | Validate an API key and respond to a ping (anal JSON parser version)
ping2 :: MandrillKey -> IO (MandrillResponse UsersResponse)
ping2 key = toMandrillResponse UsersPing2 (UsersRq key)


--------------------------------------------------------------------------------
-- | Return the senders that have tried to use this account, both verified and unverified
senders :: MandrillKey -> IO (MandrillResponse UsersResponse)
senders key = toMandrillResponse UsersSenders (UsersRq key)
