
module Network.API.Mandrill (
    module M
  , sendEmail
  , emptyMessage
  , newTextMessage
  , newHtmlMessage
  , liftIO
  ) where

import Control.Monad.Reader
import Control.Lens
import Data.Time
import Text.Blaze.Html
import Network.API.Mandrill.Types as M
import Network.API.Mandrill.Messages as M
import Network.API.Mandrill.Messages.Types as M
import Network.API.Mandrill.Trans as M
import Data.Monoid
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Aeson as JSON


--------------------------------------------------------------------------------
emptyMessage :: EmailAddress -> EmailAddress -> MandrillMessage
emptyMessage f t = MandrillMessage {
   _mmsg_html = mempty
 , _mmsg_text = Nothing
 , _mmsg_subject = T.empty
 , _mmsg_from_email = f
 , _mmsg_from_name = Nothing
 , _mmsg_to = [newRecipient t]
 , _mmsg_headers = JSON.Null
 , _mmsg_important = Nothing
 , _mmsg_track_opens = Nothing
 , _mmsg_track_clicks = Nothing
 , _mmsg_auto_text = Nothing
 , _mmsg_auto_html = Nothing
 , _mmsg_inline_css = Nothing
 , _mmsg_url_strip_qs = Nothing
 , _mmsg_preserve_recipients = Nothing
 , _mmsg_view_content_link = Nothing
 , _mmsg_bcc_address = Nothing
 , _mmsg_tracking_domain = Nothing
 , _mmsg_signing_domain = Nothing
 , _mmsg_return_path_domain = Nothing
 , _mmsg_merge = Nothing
 , _mmsg_global_merge_vars = []
 , _mmsg_merge_vars = []
 , _mmsg_tags = []
 , _mmsg_subaccount = Nothing
 , _mmsg_google_analytics_domains = []
 , _mmsg_google_analytics_campaign = Nothing
 , _mmsg_metadata = JSON.Null
 , _mmsg_recipient_metadata = []
 , _mmsg_attachments = []
 , _mmsg_images = []
  }



--------------------------------------------------------------------------------
newHtmlMessage :: EmailAddress
               -> EmailAddress
               -> T.Text
               -> Html
               -> MandrillMessage
newHtmlMessage f t subj html = let body = mkMandrillHtml html in
  ((mmsg_html .~ body) . (mmsg_subject .~ subj)) $ (emptyMessage f t)


--------------------------------------------------------------------------------
newTextMessage :: EmailAddress
               -> EmailAddress
               -> T.Text
               -> T.Text
               -> MandrillMessage
newTextMessage f t subj txt = let body = unsafeMkMandrillHtml txt in
  ((mmsg_html .~ body) .
   (mmsg_text .~ Just txt) .
   (mmsg_subject .~ subj)) (emptyMessage f t)


--------------------------------------------------------------------------------
sendEmail :: MonadIO m
          => MandrillMessage
          -> MandrillT m (MandrillResponse [MessagesResponse])
sendEmail msg = do
  key <- ask
  liftIO $ send key msg (Just True) Nothing Nothing


--------------------------------------------------------------------------------
sendTextEmail :: MonadIO m
              => MandrillMessage
              -> MandrillT m (MandrillResponse [MessagesResponse])
sendTextEmail msg = do
  key <- ask
  now <- liftIO getCurrentTime
  liftIO $ send key msg (Just True) Nothing (Just now)
