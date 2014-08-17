{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.HTTP where

import Network.API.Mandrill.Settings
import Network.API.Mandrill.Types
import qualified Data.Text as T
import Data.Monoid
import Data.Aeson
import Network.Wreq
import Control.Lens

toMandrillResponse :: (MandrillEndpoint ep, FromJSON a, ToJSON rq)
                   => ep
                   -> rq
                   -> IO (MandrillResponse a)
toMandrillResponse ep rq = do
  let fullUrl = mandrillUrl <> toUrl ep
  res <- asJSON =<< post (T.unpack fullUrl) (toJSON rq)
  return $ res ^. responseBody
