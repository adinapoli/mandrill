{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Settings where

import qualified Data.Text as T

mandrillUrl :: T.Text
mandrillUrl = "https://mandrillapp.com/api/1.0/"

data MandrillCalls =
  -- Users API
    UsersInfo
  | UsersPing
  | UsersPing2
  | UsersSenders
  -- Messages API
  | MessagesSend
  | MessagesSendTemplate
  | MessagesSearch deriving Show

class MandrillEndpoint ep where
  toUrl :: ep -> T.Text

instance MandrillEndpoint MandrillCalls where
  toUrl UsersInfo = "users/info.json"
  toUrl UsersPing = "users/ping.json"
  toUrl UsersPing2 = "users/ping2.json"
  toUrl UsersSenders = "users/senders.json"
  toUrl MessagesSend = "messages/send.json"
  toUrl MessagesSendTemplate = "messages/send-template.json"
  toUrl MessagesSearch = "messages/search.json"
