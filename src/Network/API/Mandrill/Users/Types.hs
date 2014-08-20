{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Users.Types where

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
data UsersRq = UsersRq {
    _ureq_key :: !MandrillKey
  } deriving Show

makeLenses ''UsersRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UsersRq

--------------------------------------------------------------------------------
data MandrillStats = MandrillStats {
    _msts_sent :: Int
  , _msts_hard_bounces :: Int
  , _msts_soft_bounces :: Int
  , _msts_rejects :: Int
  , _msts_complaints :: Int
  , _msts_unsubs :: Int
  , _msts_opens :: Int
  , _msts_unique_opens :: Int
  , _msts_clicks :: Int
  , _msts_unique_clicks :: Int
  } deriving Show

makeLenses ''MandrillStats
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillStats

--------------------------------------------------------------------------------
data UserStats = UserStats {
    _usts_today :: MandrillStats
  , _usts_last_7_days :: MandrillStats
  , _usts_last_30_days :: MandrillStats
  , _usts_last_60_days :: MandrillStats
  , _usts_last_90_days :: MandrillStats
  , _usts_all_time :: MandrillStats
  } deriving Show

makeLenses ''UserStats
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UserStats

--------------------------------------------------------------------------------
data UsersInfoResponse = UsersInfoResponse {
    _usir_username :: !T.Text
  , _usir_created_at :: MandrillDate
  , _usir_public_id :: !T.Text
  , _usir_reputation :: !Int
  , _usir_hourly_quota :: !Int
  , _usir_backlog :: !Int
  , _usir_stats :: UserStats
  } deriving Show

makeLenses ''UsersInfoResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UsersInfoResponse


--------------------------------------------------------------------------------
newtype UsersPingResponse = UsersPingResponse T.Text deriving Show

deriveFromJSON defaultOptions ''UsersPingResponse

instance ToJSON UsersPingResponse where
  toJSON (UsersPingResponse t) = String t


--------------------------------------------------------------------------------
data UsersPing2Response = UsersPing2Response {
    _usrr_PING :: T.Text
  } deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UsersPing2Response



--------------------------------------------------------------------------------
data UsersSendersResponse = UsersResponse {
    _usrr_address :: !T.Text
    -- ^ The sender's email address
  , _usrr_created_at :: MandrillDate
    -- ^ The date and time that the sender was first seen by Mandrill 
    -- as a UTC date string in YYYY-MM-DD HH:MM:SS format
  , _usrr_sent :: !Int
    -- ^ The total number of messages sent by this sender
  , _usrr_hard_bounces :: !Int
    -- ^ The total number of hard bounces by messages by this sender
  , _usrr_soft_bounces :: !Int
    -- ^ The total number of soft bounces by messages by this sender
  , _usrr_rejects :: !Int
    -- ^ The total number of rejected messages by this sender
  , _usrr_complaints :: !Int
    -- ^ The total number of spam complaints received
    -- for messages by this sender
  , _usrr_unsubs :: !Int
    -- ^ The total number of unsubscribe requests received
    -- for messages by this sender
  , _usrr_opens :: !Int
    -- ^ The total number of times messages by this sender have been opened
  , _usrr_clicks :: !Int
    -- ^ The total number of times tracked URLs in messages
    -- by this sender have been clicked
  , _usrr_unique_opens :: !Int
    -- ^ The number of unique opens for emails sent for this sender
  , _usrr_unique_clicks :: !Int
    -- ^ The number of unique clicks for emails sent for this sender
  } deriving Show

makeLenses ''UsersSendersResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UsersSendersResponse
