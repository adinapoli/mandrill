
module Network.API.Mandrill.Messages where

import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Messages.Types
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.HTTP
import           Data.Time
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | Send a new transactional message through Mandrill
send :: MandrillKey
     -- ^ The API key
     -> MandrillMessage
     -- ^ The email message
     -> Maybe Bool
     -- ^ Enable a background sending mode that is optimized for bulk sending
     -> Maybe T.Text
     -- ^ ip_pool
     -> Maybe UTCTime
     -- ^ send_at
     -> IO (MandrillResponse MessagesResponse)
send k msg async ip_pool send_at =
  toMandrillResponse MessagesSend (MessagesSendRq k msg async ip_pool send_at)
