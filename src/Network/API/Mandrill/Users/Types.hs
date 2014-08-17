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
    _ureq_key :: MandrillKey
  } deriving Show

makeLenses ''UsersRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UsersRq

--------------------------------------------------------------------------------
data UsersResponse = UsersResponse {
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

makeLenses ''UsersResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''UsersResponse
