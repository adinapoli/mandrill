
module Network.API.Mandrill.Messages where

import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.HTTP
import           Data.Time
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | Send a new transactional message through Mandrill
send :: MandrillKey
     -> MandrillMessage
     -> Maybe Bool
     -> Maybe T.Text
     -> Maybe UTCTime
     -> IO MandrillResponse
send k msg async ip_pool send_at =
  toMandrillResponse Send (MandrillSendRq k msg async ip_pool send_at)
