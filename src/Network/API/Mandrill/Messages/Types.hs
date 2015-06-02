{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Messages.Types where

import           Data.Char
import           Data.Time
import qualified Data.Text as T
import           Control.Lens
import           Control.Monad
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH

import           Network.API.Mandrill.Types


--------------------------------------------------------------------------------
data MessagesSendRq = MessagesSendRq {
    _msrq_key :: MandrillKey
  , _msrq_message :: MandrillMessage
  , _msrq_async :: Maybe Bool
  , _msrq_ip_pool :: Maybe T.Text
  , _msrq_send_at :: Maybe UTCTime
  } deriving Show

makeLenses ''MessagesSendRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MessagesSendRq

--------------------------------------------------------------------------------
data MessagesSendTemplateRq = MessagesSendTemplateRq {
    _mstrq_key              :: MandrillKey
  , _mstrq_template_name    :: T.Text
  , _mstrq_template_content :: [MandrillTemplateContent]
  , _mstrq_message          :: MandrillMessage
  , _mstrq_async            :: Maybe Bool
  , _mstrq_ip_pool          :: Maybe T.Text
  , _mstrq_send_at          :: Maybe UTCTime
  } deriving Show

makeLenses ''MessagesSendTemplateRq
deriveJSON defaultOptions { fieldLabelModifier = drop 7 } ''MessagesSendTemplateRq

--------------------------------------------------------------------------------
data MessagesResponse = MessagesResponse {
    _mres_email :: !T.Text
    -- ^ The email address of the recipient
  , _mres_status :: MandrillEmailStatus
    -- ^ The sending status of the recipient
  , _mres_reject_reason :: Maybe MandrillRejectReason
    -- ^ The reason for the rejection if the recipient status is "rejected"
  , _mres__id    :: !T.Text
    -- ^ The message's unique id
  } deriving Show

makeLenses ''MessagesResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MessagesResponse
