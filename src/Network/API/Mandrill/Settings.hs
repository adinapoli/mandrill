{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Settings where

import qualified Data.Text as T

mandrillUrl :: T.Text
mandrillUrl = "https://mandrillapp.com/api/1.0/"

data MandrillCalls =
    Send
  | SendTemplate
  | Search deriving Show

class MandrillEndpoint ep where
  toUrl :: ep -> T.Text

instance MandrillEndpoint MandrillCalls where
  toUrl Send = "messages/send.json"
  toUrl SendTemplate = "messages/send-template.json"
  toUrl Search = "messages/search.json"
