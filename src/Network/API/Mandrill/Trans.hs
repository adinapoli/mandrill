{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.API.Mandrill.Trans where

import Control.Monad.Reader
import Control.Applicative
import Network.API.Mandrill.Types

newtype MandrillT m a = MandrillT {
  runMandrillT :: ReaderT MandrillKey m a
  } deriving (MonadTrans, MonadReader MandrillKey,
              Functor, Applicative, Monad, MonadIO)

runMandrill :: MonadIO m
            => MandrillKey
            -> MandrillT m a
            -> m a
runMandrill key action = runReaderT (runMandrillT action) key
