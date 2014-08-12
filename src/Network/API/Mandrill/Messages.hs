
module Network.API.Mandrill.Messages where

import Network.API.Mandrill.Types
import Network.API.Mandrill.Settings
import Network.API.Mandrill.HTTP

send :: MandrillKey -> MandrillMessage -> Bool -> IO MandrillResponse
send k msg async = toMandrillResponse Send (MandrillSendRq k msg async)
