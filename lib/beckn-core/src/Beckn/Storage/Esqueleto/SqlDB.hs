{-# LANGUAGE DerivingStrategies #-}

module Beckn.Storage.Esqueleto.SqlDB
  ( SqlDBEnv (..),
    SqlDB,
  )
where

import Beckn.Storage.Esqueleto.Logger (LoggerIO)
import Beckn.Types.Time (MonadTime (..))
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental (SqlBackend)
import EulerHS.Prelude

newtype SqlDBEnv = SqlDBEnv
  { currentTime :: UTCTime
  }

type SqlDB a = ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO) a

instance Monad m => MonadTime (ReaderT SqlDBEnv m) where
  getCurrentTime = asks (.currentTime)
