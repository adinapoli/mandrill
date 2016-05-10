{-# LANGUAGE TemplateHaskell #-}
module Network.API.Mandrill.Senders where
import           Control.Lens
import           Data.Aeson                    (FromJSON, ToJSON, parseJSON,
                                                toJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Aeson.Types              (Value (..), fieldLabelModifier)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Network.API.Mandrill.HTTP     (toMandrillResponse)
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Types
import           Network.HTTP.Client           (Manager)
import qualified Text.Email.Validate           as TEV

data VerifyDomainRq =
  VerifyDomainRq
  { _vdrq_key     :: MandrillKey
  , _vdrq_domain  :: Text
  , _vdrq_mailbox :: Text
  } deriving Show

makeLenses ''VerifyDomainRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''VerifyDomainRq

data VerifyDomainResponse =
  VerifyDomainResponse
  { _vdres_status :: Text
  , _vdres_domain :: Text
  , _vdres_email  :: TEV.EmailAddress
  } deriving Show

makeLenses ''VerifyDomainResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''VerifyDomainResponse


verifyDomain :: MandrillKey
                -- ^ The API key
                -> TEV.EmailAddress
                -- ^ Email address to use for verification
                -> Maybe Manager
                -> IO (MandrillResponse VerifyDomainResponse)
verifyDomain k email =
  toMandrillResponse VerifyDomain
  (VerifyDomainRq k (decodeUtf8 $ TEV.domainPart email) (decodeUtf8 $ TEV.localPart email))
-- addDomain :: MandrillKey

-- addDomain k dom = toMandrillResponse DomainsAdd (DomainAddRq k dom)



-- addRoute :: MandrillKey
--      -- ^ The API key
--      -> Text
--      -- ^ The domain to add
--      -> Text
--      -- ^ the pattern including wildcards
--      -> Text
--      -- ^ URL to forward to
--      -> Maybe Manager
--      -> IO (MandrillResponse RouteAddResponse)
-- addRoute k dom pattern forward = toMandrillResponse RoutesAdd (RouteAddRq k dom pattern forward)
