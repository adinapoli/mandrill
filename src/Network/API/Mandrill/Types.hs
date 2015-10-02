{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Types where

import           Network.API.Mandrill.Utils
import           Test.QuickCheck
import           Text.Email.Validate
import           Data.Char
import           Data.Maybe
import           Data.Time
import           Control.Applicative
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (TimeLocale, defaultTimeLocale)
#else
import System.Locale (TimeLocale, defaultTimeLocale)
#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as TL
import qualified Data.Text.Lazy as TL
import           Control.Lens
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze


timeParse :: ParseTime t => TimeLocale -> String -> String -> Maybe t
#if MIN_VERSION_time(1,5,0)
timeParse = parseTimeM True
#else
timeParse = parseTime
#endif

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
    MandrillSuccess k
  | MandrillFailure MandrillError deriving Show

instance FromJSON k => FromJSON (MandrillResponse k) where
  parseJSON v = case (parseMaybe parseJSON v) :: Maybe k of
    Just r -> return $ MandrillSuccess r
    Nothing -> do
    -- try to parse it as an error
      case (parseMaybe parseJSON v) :: Maybe MandrillError of
        Just e -> return $ MandrillFailure e
        Nothing -> fail $ show v <> " is neither a MandrillSuccess or a MandrillError."


--------------------------------------------------------------------------------
data MandrillRecipientTag = To | Cc | Bcc deriving Show

deriveJSON defaultOptions { constructorTagModifier = map toLower } ''MandrillRecipientTag


--------------------------------------------------------------------------------
newtype MandrillEmail = MandrillEmail EmailAddress deriving Show

instance ToJSON MandrillEmail where
  toJSON (MandrillEmail e) = String . TL.decodeUtf8 . toByteString $ e

instance FromJSON MandrillEmail where
  parseJSON (String s) = case validate (TL.encodeUtf8 s) of
    Left err -> fail err
    Right v  -> return . MandrillEmail $ v
  parseJSON o = typeMismatch "Expecting a String for MandrillEmail." o


--------------------------------------------------------------------------------
-- | An array of recipient information.
data MandrillRecipient = MandrillRecipient {
    _mrec_email :: MandrillEmail
    -- ^ The email address of the recipient
  , _mrec_name :: Maybe T.Text
    -- ^ The optional display name to use for the recipient
  , _mrec_type :: Maybe MandrillRecipientTag
    -- ^ The header type to use for the recipient.
    --   defaults to "to" if not provided
  } deriving Show

makeLenses ''MandrillRecipient
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillRecipient

newRecipient :: EmailAddress -> MandrillRecipient
newRecipient email = MandrillRecipient (MandrillEmail email) Nothing Nothing

instance Arbitrary MandrillRecipient where
  arbitrary = pure MandrillRecipient {
      _mrec_email = MandrillEmail $ fromJust (emailAddress "test@example.com")
    , _mrec_name  =  Nothing
    , _mrec_type  =  Nothing
    }

--------------------------------------------------------------------------------
newtype MandrillHtml = MandrillHtml Blaze.Html

unsafeMkMandrillHtml :: T.Text -> MandrillHtml
unsafeMkMandrillHtml = MandrillHtml . Blaze.preEscapedToHtml

-- This might be slightly hairy because it violates
-- the nice encapsulation that newtypes offer.
mkMandrillHtml :: Blaze.Html -> MandrillHtml
mkMandrillHtml = MandrillHtml

instance Monoid MandrillHtml where
  mempty = MandrillHtml mempty
  mappend (MandrillHtml m1) (MandrillHtml m2) = MandrillHtml (m1 <> m2)

instance Show MandrillHtml where
  show (MandrillHtml h) = show $ Blaze.renderHtml h

instance ToJSON MandrillHtml where
  toJSON (MandrillHtml h) = String . TL.toStrict . Blaze.renderHtml $ h

instance FromJSON MandrillHtml where
  parseJSON (String h) = return $ MandrillHtml (Blaze.preEscapedToHtml h)
  parseJSON v = typeMismatch "Expecting a String for MandrillHtml" v

instance Arbitrary MandrillHtml where
  arbitrary = pure $ mkMandrillHtml "<p><b>FooBar</b></p>"

--------------------------------------------------------------------------------
type MandrillTags = T.Text


--------------------------------------------------------------------------------
type MandrillHeaders = Object


--------------------------------------------------------------------------------

data MergeVar = MergeVar {
      _mv_name    :: !T.Text
    , _mv_content :: Value
    } deriving Show

makeLenses ''MergeVar
deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''MergeVar

--------------------------------------------------------------------------------
data MandrillMergeVars = MandrillMergeVars {
    _mmvr_rcpt :: !T.Text
  , _mmvr_vars :: [MergeVar]
  } deriving Show

makeLenses ''MandrillMergeVars
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillMergeVars

--------------------------------------------------------------------------------
data MandrillMetadata = MandrillMetadata {
    _mmdt_rcpt :: !T.Text
  , _mmdt_values :: Object
  } deriving Show

makeLenses ''MandrillMetadata
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillMetadata


data Base64ByteString =
    EncodedB64BS B.ByteString
  -- ^ An already-encoded Base64 ByteString.
  | PlainBS B.ByteString
  -- ^ A plain Base64 ByteString which requires encoding.
  deriving Show

instance ToJSON Base64ByteString where
  toJSON (PlainBS bs)      = String . TL.decodeUtf8 . Base64.encode $ bs
  toJSON (EncodedB64BS bs) = String . TL.decodeUtf8 $ bs

instance FromJSON Base64ByteString where
  parseJSON (String v) = pure $ EncodedB64BS (TL.encodeUtf8 v)
  parseJSON rest = typeMismatch "Base64ByteString must be a String." rest

--------------------------------------------------------------------------------
data MandrillWebContent = MandrillWebContent {
    _mwct_type :: !T.Text
  , _mwct_name :: !T.Text
    -- ^ [for images] the Content ID of the image
    -- - use <img src="cid:THIS_VALUE"> to reference the image
    -- in your HTML content
  , _mwct_content :: !Base64ByteString
  } deriving Show

makeLenses ''MandrillWebContent
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillWebContent

--------------------------------------------------------------------------------
-- | The information on the message to send
data MandrillMessage = MandrillMessage {
   _mmsg_html :: MandrillHtml
   -- ^ The full HTML content to be sent
 , _mmsg_text :: Maybe T.Text
   -- ^ Optional full text content to be sent
 , _mmsg_subject :: !T.Text
   -- ^ The message subject
 , _mmsg_from_email :: MandrillEmail
   -- ^ The sender email address
 , _mmsg_from_name :: Maybe T.Text
   -- ^ Optional from name to be used
 , _mmsg_to :: [MandrillRecipient]
   -- ^ A list of recipient information
 , _mmsg_headers :: MandrillHeaders
   -- ^ optional extra headers to add to the message (most headers are allowed)
 , _mmsg_important :: Maybe Bool
   -- ^ whether or not this message is important, and should be delivered ahead
   -- of non-important messages
 , _mmsg_track_opens :: Maybe Bool
   -- ^ whether or not to turn on open tracking for the message
 , _mmsg_track_clicks :: Maybe Bool
   -- ^ whether or not to turn on click tracking for the message
 , _mmsg_auto_text :: Maybe Bool
   -- ^ whether or not to automatically generate a text part for messages that are not given text
 , _mmsg_auto_html :: Maybe Bool
   -- ^ whether or not to automatically generate an HTML part for messages that are not given HTML
 , _mmsg_inline_css :: Maybe Bool
   -- ^ whether or not to automatically inline all CSS styles provided in the message HTML
   -- - only for HTML documents less than 256KB in size
 , _mmsg_url_strip_qs :: Maybe Bool
   -- ^ whether or not to strip the query string from URLs when aggregating tracked URL data
 , _mmsg_preserve_recipients :: Maybe Bool
   -- ^ whether or not to expose all recipients in to "To" header for each email
 , _mmsg_view_content_link :: Maybe Bool
   -- ^ set to false to remove content logging for sensitive emails
 , _mmsg_bcc_address :: Maybe T.Text
   -- ^ an optional address to receive an exact copy of each recipient's email
 , _mmsg_tracking_domain :: Maybe T.Text
   -- ^ a custom domain to use for tracking opens and clicks instead of mandrillapp.com
 , _mmsg_signing_domain :: Maybe Bool
   -- ^ a custom domain to use for SPF/DKIM signing instead of mandrill
   -- (for "via" or "on behalf of" in email clients)
 , _mmsg_return_path_domain :: Maybe Bool
   -- ^ a custom domain to use for the messages's return-path
 , _mmsg_merge :: Maybe Bool
   -- ^ whether to evaluate merge tags in the message.
   -- Will automatically be set to true if either merge_vars
   -- or global_merge_vars are provided.
 , _mmsg_global_merge_vars :: [MergeVar]
   -- ^ global merge variables to use for all recipients. You can override these per recipient.
 , _mmsg_merge_vars :: [MandrillMergeVars]
   -- ^ per-recipient merge variables, which override global merge variables with the same name.
 , _mmsg_tags :: [MandrillTags]
   -- ^ an array of string to tag the message with. Stats are accumulated using tags,
   -- though we only store the first 100 we see, so this should not be unique
   -- or change frequently. Tags should be 50 characters or less.
   -- Any tags starting with an underscore are reserved for internal use
   -- and will cause errors.
 , _mmsg_subaccount :: Maybe T.Text
   -- ^ the unique id of a subaccount for this message
   -- - must already exist or will fail with an error
 , _mmsg_google_analytics_domains :: [T.Text]
   -- ^ an array of strings indicating for which any matching URLs
   -- will automatically have Google Analytics parameters appended
   -- to their query string automatically.
 , _mmsg_google_analytics_campaign :: Maybe T.Text
   -- ^ optional string indicating the value to set for the utm_campaign
   -- tracking parameter. If this isn't provided the email's from address
   -- will be used instead.
 , _mmsg_metadata :: Object
   -- ^ metadata an associative array of user metadata. Mandrill will store
   -- this metadata and make it available for retrieval.
   -- In addition, you can select up to 10 metadata fields to index
   -- and make searchable using the Mandrill search api.
 , _mmsg_recipient_metadata :: [MandrillMetadata]
   -- ^ Per-recipient metadata that will override the global values
   -- specified in the metadata parameter.
 , _mmsg_attachments :: [MandrillWebContent]
   -- ^ an array of supported attachments to add to the message
 , _mmsg_images :: [MandrillWebContent]
   -- ^ an array of embedded images to add to the message
 } deriving Show

makeLenses ''MandrillMessage
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillMessage

instance Arbitrary MandrillMessage where
  arbitrary = MandrillMessage <$> arbitrary
                              <*> pure Nothing
                              <*> pure "Test Subject"
                              <*> pure (MandrillEmail . fromJust $ emailAddress "sender@example.com")
                              <*> pure Nothing
                              <*> resize 2 arbitrary
                              <*> pure H.empty
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure Nothing
                              <*> pure []
                              <*> pure []
                              <*> pure []
                              <*> pure Nothing
                              <*> pure []
                              <*> pure Nothing
                              <*> pure H.empty
                              <*> pure []
                              <*> pure []
                              <*> pure []

--------------------------------------------------------------------------------
-- | Key value pair for replacing content in templates via 'Editable Regions'
data MandrillTemplateContent = MandrillTemplateContent {
    _mtc_name    :: T.Text
  , _mtc_content :: T.Text
  } deriving Show

makeLenses ''MandrillTemplateContent
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''MandrillTemplateContent

--------------------------------------------------------------------------------
type MandrillKey = T.Text
type MandrillTemplate = T.Text

newtype MandrillDate = MandrillDate {
  fromMandrillDate :: UTCTime
  } deriving Show

instance ToJSON MandrillDate where
  toJSON = toJSON . fromMandrillDate

instance FromJSON MandrillDate where
  parseJSON = withText "MandrillDate" $ \t ->
      case timeParse defaultTimeLocale "%Y-%m-%d %I:%M:%S%Q" (T.unpack t) of
        Just d -> pure $ MandrillDate d
        _      -> fail "could not parse Mandrill date"
