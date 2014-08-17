{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Types where

import           Network.API.Mandrill.Utils
import           Data.Char
import           Data.Time
import           Control.Applicative
import           System.Locale (defaultTimeLocale)
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
-- | The main datatypes which models the response from the Mandrill API,
-- which can be either a success or a failure.
data MandrillResponse k =
    MandrillSuccess [k]
  | MandrillFailure MandrillError deriving Show

instance FromJSON k => FromJSON (MandrillResponse k) where
  parseJSON a@(Array _) = case (parseMaybe parseJSON a) :: Maybe [k] of
    Just r -> return $ MandrillSuccess r
    Nothing -> fail $ show a <> " is not a valid MandrillSuccess"
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
type MandrillKey = T.Text

newtype MandrillDate = MandrillDate {
  fromMandrillDate :: UTCTime
  } deriving Show

instance ToJSON MandrillDate where
  toJSON = toJSON . fromMandrillDate

instance FromJSON MandrillDate where
  parseJSON = withText "MandrillDate" $ \t ->
      case parseTime defaultTimeLocale "%Y-%m-%d %I:%M:%S%Q" (T.unpack t) of
        Just d -> pure $ MandrillDate d
        _      -> fail "could not parse Mandrill date"
