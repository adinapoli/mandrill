{-# LANGUAGE TemplateHaskell #-}
module Network.API.Mandrill.Inbound where
import           Data.Aeson                    (FromJSON, ToJSON, parseJSON,
                                                toJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Aeson.Types              (fieldLabelModifier)
import           Data.Text                     (Text)
import           Lens.Micro.TH                 (makeLenses)
import           Network.API.Mandrill.HTTP     (toMandrillResponse)
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Types
import           Network.HTTP.Client           (Manager)

data DomainAddRq =
  DomainAddRq
  { _darq_key    :: MandrillKey
  , _darq_domain :: Text
  } deriving Show

makeLenses ''DomainAddRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''DomainAddRq

data DomainAddResponse =
  DomainAddResponse
  { _dares_domain     :: Text
  , _dares_created_at :: MandrillDate
  , _dares_valid_mx   :: Bool
  } deriving Show



makeLenses ''DomainAddResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''DomainAddResponse

data RouteAddResponse =
  RouteAddResponse
  { _rares_id      :: Text
  , _rares_pattern :: Text
  , _rares_url     :: Text
  } deriving Show



makeLenses ''RouteAddResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''RouteAddResponse

data RouteAddRq =
  RouteAddRq
  { _rarq_key     :: Text
  , _rarq_domain  :: Text
  , _rarq_pattern :: Text
  , _rarq_url     :: Text
  } deriving Show

makeLenses ''RouteAddRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''RouteAddRq

addDomain :: MandrillKey
     -- ^ The API key
     -> Text
     -- ^ The domain to add
     -> Maybe Manager
     -> IO (MandrillResponse DomainAddResponse)
addDomain k dom = toMandrillResponse DomainsAdd (DomainAddRq k dom)



addRoute :: MandrillKey
     -- ^ The API key
     -> Text
     -- ^ The domain to add
     -> Text
     -- ^ the pattern including wildcards
     -> Text
     -- ^ URL to forward to
     -> Maybe Manager
     -> IO (MandrillResponse RouteAddResponse)
addRoute k dom pattern forward = toMandrillResponse RoutesAdd (RouteAddRq k dom pattern forward)
