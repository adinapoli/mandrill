
module Network.API.Mandrill.Orphans where

import Data.Aeson
import Data.Aeson.Types
import Text.Email.Validate
import qualified Data.Text.Encoding as TE


instance ToJSON EmailAddress where
  toJSON = String . TE.decodeUtf8 . toByteString

instance FromJSON EmailAddress where
  parseJSON (String s) = case validate (TE.encodeUtf8 s) of
    Left err -> fail err
    Right v  -> return v
  parseJSON o = typeMismatch "Expecting a String for EmailAddress." o
