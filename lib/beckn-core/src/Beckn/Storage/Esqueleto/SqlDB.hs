{-# LANGUAGE DerivingStrategies #-}

module Beckn.Storage.Esqueleto.SqlDB
  ( SqlDBEnv (..),
    SqlDB,
  )
where

import Beckn.Storage.Esqueleto.Logger (LoggerIO)
import Beckn.Types.GuidLike
import Beckn.Types.MonadGuid
import Beckn.Types.Time (MonadTime (..))
import Beckn.Utils.Logging
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (SqlBackend)
import EulerHS.Prelude

newtype SqlDBEnv = SqlDBEnv
  { currentTime :: UTCTime
  }

type SqlDB a = ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO) a

instance Monad m => MonadTime (ReaderT SqlDBEnv m) where
  getCurrentTime = asks (.currentTime)

instance MonadGuid (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  generateGUIDText = lift $ lift generateGUID

instance Log (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  logOutput a b = lift . lift $ logOutput a b
  withLogTag a (ReaderT f1) = ReaderT $ \env1 -> do
    let (ReaderT f2) = f1 env1
    ReaderT $ \env2 ->
      withLogTag a $ f2 env2
