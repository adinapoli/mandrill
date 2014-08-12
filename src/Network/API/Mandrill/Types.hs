{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Types where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze

data MandrillError = MandrillError {
    _merr_status :: !T.Text
  , _merr_code :: !Int
  , _merr_name :: !T.Text
  , _merr_message :: !T.Text
  } deriving Show

makeLenses ''MandrillError
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillError

data MandrillResult = MandrillResult {
    _mres_email :: !T.Text
  , _mres_status :: !T.Text
  , _mres__id    :: !T.Text
  , _mres_reject_reason :: Maybe T.Text
  } deriving Show

makeLenses ''MandrillResult
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillResult

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

data MandrillRecipient = MandrillRecipient {
    _mrec_email :: !T.Text
  , _mrec_name :: !T.Text
  , _mrec_type :: !T.Text
  } deriving Show

makeLenses ''MandrillRecipient
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillRecipient

newtype MandrillHtml = MandrillHtml Blaze.Html

mkMandrillHtml :: T.Text -> MandrillHtml
mkMandrillHtml = MandrillHtml . Blaze.toHtml

instance Show MandrillHtml where
  show (MandrillHtml h) = show $ Blaze.renderHtml h

instance ToJSON MandrillHtml where
  toJSON (MandrillHtml h) = String . TL.toStrict . Blaze.renderHtml $ h

instance FromJSON MandrillHtml where
  parseJSON (String h) = return $ MandrillHtml (Blaze.toHtml h)
  parseJSON v = typeMismatch "Expecting a String for MandrillHtml" v

data MandrillMessage = MandrillMessage {
   _mmsg_html :: MandrillHtml
 , _mmsg_text :: !T.Text
 , _mmsg_subject :: !T.Text
 , _mmsg_from_email :: !T.Text
 , _mmsg_from_name :: !T.Text
 , _mmsg_to :: [MandrillRecipient]
 } deriving Show

makeLenses ''MandrillMessage
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillMessage

-- * Send payloads

type MandrillKey = T.Text

data MandrillSendRq = MandrillSendRq {
    _msrq_key :: MandrillKey
  , _msrq_message :: MandrillMessage
  , _msrq_async :: Bool
  } deriving Show

makeLenses ''MandrillSendRq
deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''MandrillSendRq
