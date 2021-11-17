{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Storage.Esqueleto.Logger (LoggerIO (..), runLoggerIO) where

import Beckn.Types.Logging (LogLevel (..), LoggerEnv)
import Beckn.Types.Time (MonadTime (..))
import Beckn.Utils.IOLogging (logOutputIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger as CMLogger
  ( Loc,
    LogLevel (..),
    LogSource,
    MonadLogger (..),
    MonadLoggerIO (..),
    ToLogStr (toLogStr),
    fromLogStr,
  )
import EulerHS.Prelude hiding (Key)

--TODO: Remove this when we remove EulerHS
newtype LoggerIO a = LoggerIO (ReaderT LoggerEnv IO a)
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

instance MonadTime LoggerIO where
  getCurrentTime = liftIO getCurrentTime

runLoggerIO :: LoggerEnv -> LoggerIO a -> IO a
runLoggerIO logEnv (LoggerIO rdr) = runReaderT rdr logEnv

logFunc :: ToLogStr msg => LoggerEnv -> Loc -> LogSource -> CMLogger.LogLevel -> msg -> IO ()
logFunc logEnv _ _ logLevel msg =
  logOutputIO logEnv logLevel' . decodeUtf8 . fromLogStr $ toLogStr msg
  where
    logLevel' = case logLevel of
      LevelError -> ERROR
      LevelWarn -> WARNING
      LevelDebug -> DEBUG
      _ -> INFO

instance MonadLogger LoggerIO where
  monadLoggerLog loc logSource logLevel msg = LoggerIO $ do
    loggerEnv <- ask
    liftIO $ logFunc loggerEnv loc logSource logLevel msg

instance MonadLoggerIO LoggerIO where
  askLoggerIO =
    LoggerIO $
      logFunc <$> ask