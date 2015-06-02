
module Network.API.Mandrill.Messages where

import           Network.API.Mandrill.Types
import           Network.API.Mandrill.Messages.Types
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.HTTP
import           Network.HTTP.Client
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
     -> Maybe Manager
     -> IO (MandrillResponse [MessagesResponse])
send k msg async ip_pool send_at = toMandrillResponse MessagesSend (MessagesSendRq k msg async ip_pool send_at)

-- | Send a new transactional message through Mandrill using a template
sendTemplate :: MandrillKey
             -- ^ The API key
             -> MandrillTemplate
             -- ^ The template name
             -> [MandrillTemplateContent]
             -- ^ Template content for 'editable regions'
             -> MandrillMessage
             -- ^ The email message
             -> Maybe Bool
             -- ^ Enable a background sending mode that is optimized for bulk sending
             -> Maybe T.Text
             -- ^ ip_pool
             -> Maybe UTCTime
             -- ^ send_at
             -> Maybe Manager
             -> IO (MandrillResponse [MessagesResponse])
sendTemplate k template content msg async ip_pool send_at = toMandrillResponse MessagesSendTemplate (MessagesSendTemplateRq k template content msg async ip_pool send_at)
