{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.API.Mandrill.Trans where

import Control.Monad.Reader
import Control.Applicative
import Network.API.Mandrill.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS


--------------------------------------------------------------------------------
newtype MandrillT m a = MandrillT {
  runMandrillT :: ReaderT (MandrillKey, Manager) m a
  } deriving (MonadTrans, MonadReader (MandrillKey, Manager),
              Functor, Applicative, Monad, MonadIO)


--------------------------------------------------------------------------------
type Mandrill = MandrillT IO


--------------------------------------------------------------------------------
runMandrill :: MonadIO m
            => MandrillKey
            -> MandrillT m a
            -> m a
runMandrill key action = do
  mgr <- liftIO $ newManager tlsManagerSettings
  runReaderT (runMandrillT action) (key, mgr)
