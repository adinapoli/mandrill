{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Types where

import           Network.API.Mandrill.Utils
import           Data.Char
import           Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Control.Lens
import           Control.Monad
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze


--------------------------------------------------------------------------------
data MandrillError = MandrillError {
    _merr_status :: !T.Text
  , _merr_code :: !Int
  , _merr_name :: !T.Text
  , _merr_message :: !T.Text
  } deriving Show

makeLenses ''MandrillError
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillError


--------------------------------------------------------------------------------
data MandrillEmailStatus = ES_Sent
                         | ES_Queued
                         | ES_Scheduled
                         | ES_Rejected
                         | ES_Invalid deriving Show

deriveJSON defaultOptions { constructorTagModifier = map toLower . drop 3 } ''MandrillEmailStatus


--------------------------------------------------------------------------------
data MandrillRejectReason = RR_HardBounce
                          | RR_SoftBounce
                          | RR_Spam
                          | RR_Unsub
                          | RR_Custom
                          | RR_InvalidSender
                          | RR_Invalid
                          | RR_TestModeLimit
                          | RR_Rule deriving Show

deriveJSON defaultOptions {
  constructorTagModifier = modRejectReason . drop 3
  } ''MandrillRejectReason


--------------------------------------------------------------------------------
data MandrillResult = MandrillResult {
    _mres_email :: !T.Text
    -- ^ The email address of the recipient
  , _mres_status :: MandrillEmailStatus
    -- ^ The sending status of the recipient
  , _mres_reject_reason :: Maybe MandrillRejectReason
    -- ^ The reason for the rejection if the recipient status is "rejected"
  , _mres__id    :: !T.Text
    -- ^ The message's unique id
  } deriving Show

makeLenses ''MandrillResult
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillResult

--------------------------------------------------------------------------------
-- | The main datatypes which models the response from the Mandrill API,
-- which can be either a success or a failure.
data MandrillResponse =
    MandrillSuccess [MandrillResult]
  | MandrillFailure MandrillError deriving Show

instance FromJSON MandrillResponse where
  parseJSON a@(Array _) = case (parseMaybe parseJSON a) :: Maybe [MandrillResult] of
    Just r -> return $ MandrillSuccess r
    Nothing -> fail $ show a <> " is not a valid MandrillResult"
  parseJSON o@(Object _) = case (parseMaybe parseJSON o) :: Maybe MandrillError of
    Just e -> return $ MandrillFailure e
    Nothing -> fail $ show o <> " is not a valid MandrillError"
  parseJSON v = typeMismatch "Expected an Object or an Array for MandrillResponse" v


--------------------------------------------------------------------------------
data MandrillRecipientTag = To | Cc | Bcc deriving Show

deriveJSON defaultOptions { constructorTagModifier = map toLower } ''MandrillRecipientTag


--------------------------------------------------------------------------------
-- | An array of recipient information.
data MandrillRecipient = MandrillRecipient {
    _mrec_email :: !T.Text
    -- ^ The email address of the recipient
  , _mrec_name :: Maybe T.Text
    -- ^ The optional display name to use for the recipient
  , _mrec_type :: Maybe MandrillRecipientTag
    -- ^ The header type to use for the recipient.
    --   defaults to "to" if not provided
  } deriving Show

makeLenses ''MandrillRecipient
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillRecipient


--------------------------------------------------------------------------------
newtype MandrillHtml = MandrillHtml Blaze.Html

mkMandrillHtml :: T.Text -> MandrillHtml
mkMandrillHtml = MandrillHtml . Blaze.toHtml

mkMandrillHtml' :: Blaze.Html -> MandrillHtml
mkMandrillHtml' = MandrillHtml

instance Show MandrillHtml where
  show (MandrillHtml h) = show $ Blaze.renderHtml h

instance ToJSON MandrillHtml where
  toJSON (MandrillHtml h) = String . TL.toStrict . Blaze.renderHtml $ h

instance FromJSON MandrillHtml where
  parseJSON (String h) = return $ MandrillHtml (Blaze.toHtml h)
  parseJSON v = typeMismatch "Expecting a String for MandrillHtml" v


--------------------------------------------------------------------------------
-- | The information on the message to send
data MandrillMessage = MandrillMessage {
   _mmsg_html :: MandrillHtml
   -- ^ The full HTML content to be sent
 , _mmsg_text :: Maybe T.Text
   -- ^ Optional full text content to be sent
 , _mmsg_subject :: !T.Text
   -- ^ The message subject
 , _mmsg_from_email :: !T.Text
   -- ^ The sender email address
 , _mmsg_from_name :: Maybe T.Text
   -- ^ Optional from name to be used
 , _mmsg_to :: [MandrillRecipient]
   -- ^ A list of recipient information
 } deriving Show

makeLenses ''MandrillMessage
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillMessage


--------------------------------------------------------------------------------
-- * Send payloads
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
type MandrillKey = T.Text


--------------------------------------------------------------------------------
data MandrillSendRq = MandrillSendRq {
    _msrq_key :: MandrillKey
  , _msrq_message :: MandrillMessage
  , _msrq_async :: Maybe Bool
  , _msrq_ip_pool :: Maybe T.Text
  , _msrq_send_at :: Maybe UTCTime
  } deriving Show

makeLenses ''MandrillSendRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillSendRq
