{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.API.Mandrill.Webhooks where
import           Control.Applicative           (pure)
import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON, ToJSON, parseJSON,
                                                toJSON)
import           Data.Aeson                    (Value (String))
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Aeson.Types              (fieldLabelModifier)
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro.TH                 (makeLenses)
import           Network.API.Mandrill.HTTP     (toMandrillResponse)
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Types
import           Network.HTTP.Client           (Manager)

data EventHook
  = EventSent
  | EventDeferred
  | EventHardBounced
  | EventSoftBounced
  | EventOpened
  | EventClicked
  | EventMarkedAsSpam
  | EventUnsubscribed
  | EventRejected
  deriving (Ord,Eq)

instance Show EventHook where
  show e = case e of
    EventSent         -> "send"
    EventDeferred     -> "deferral"
    EventSoftBounced  -> "soft_bounce"
    EventHardBounced  -> "hard_bounce"
    EventOpened       -> "open"
    EventClicked      -> "click"
    EventMarkedAsSpam -> "spam"
    EventUnsubscribed -> "unsub"
    EventRejected     -> "reject"
instance FromJSON EventHook where
  parseJSON (String s) =
    case s of
      "send"           -> pure EventSent
      "deferral"       -> pure EventDeferred
      "soft_bounce"    -> pure EventSoftBounced
      "hard_bounce"    -> pure EventHardBounced
      "open"           -> pure EventOpened
      "click"          -> pure EventClicked
      "spam"           -> pure EventMarkedAsSpam
      "reject"         -> pure EventRejected
      "unsub"          -> pure EventUnsubscribed
      x                -> fail ("can't parse " ++  show x)


instance ToJSON EventHook where
  toJSON e = String (T.pack $ show e)

data WebhookAddRq =
  WebhookAddRq
  { _warq_key         :: MandrillKey
  , _warq_url         :: Text
  , _warq_description :: Text
  , _warq_events      :: Set EventHook
  } deriving Show

makeLenses ''WebhookAddRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''WebhookAddRq
