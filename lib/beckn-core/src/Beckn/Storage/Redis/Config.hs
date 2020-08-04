{-# LANGUAGE TypeApplications #-}

module Beckn.Storage.Redis.Config where

import Beckn.Types.App
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import System.Environment

loadRedisConfig :: IO (Maybe T.KVDBConfig)
loadRedisConfig = do
  mhost <- lookupEnv "REDIS_HOST"
  mport <- lookupEnv "REDIS_PORT"
  _mauth <- lookupEnv "REDIS_AUTH"
  mdb <- lookupEnv "REDIS_DB"
  mmaxConnections <- lookupEnv "REDIS_MAX_CONNECTIONS"
  mmaxIdleTime <- lookupEnv "REDIS_MAX_IDLE_TIME"
  mtimeout <- lookupEnv "REDIS_CONNECTION_TIMEOUT"
  pure $ do
    host <- mhost
    port <- mport
    db <- mdb
    maxConnections <- mmaxConnections
    maxIdleTime <- mmaxIdleTime
    p <- readMaybe port
    Just $
      T.mkKVDBConfig "redis" $
        T.RedisConfig
          { connectHost = host,
            connectPort = p,
            connectAuth = Nothing, -- FIXME: this should use auth
            connectDatabase = read db,
            connectMaxConnections = read maxConnections,
            connectMaxIdleTime = fromRational . toRational @Integer $ read maxIdleTime,
            connectTimeout = fromRational . toRational @Integer . read <$> mtimeout
          }

prepareRedisConnections :: (L.MonadFlow mFlow, HasRedisEnv mFlow) => mFlow ()
prepareRedisConnections = do
  mConfig <- L.runIO loadRedisConfig
  redisEnv <- getRedisEnv
  let defKVDBConfig = T.mkKVDBConfig "redis" $ defaultRedisConfig redisEnv
  let kvDBConfig' = fromMaybe defKVDBConfig mConfig
  kvConn <- L.getOrInitKVDBConn kvDBConfig'
  throwOnFailedWithLog
    kvConn
    KVDBConnectionFailedException
    "Failed to get or initialize connection to Redis."
  eitherResp <- L.runKVDB "redis" $ L.setex "dummy" 1 "dummy"
  case eitherResp of
    Left _ ->
      throwFailedWithLog
        KVDBConnectionFailedException
        "Failed to get or initialize connection to Redis."
    Right _ -> pure ()

throwOnFailedWithLog ::
  (L.MonadFlow mFlow, Show e) =>
  Either e a ->
  (Text -> AppException) ->
  Text ->
  mFlow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError ("" :: Text) $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
throwOnFailedWithLog _ _ _ = pure ()

throwFailedWithLog :: L.MonadFlow mFlow => (Text -> AppException) -> Text -> mFlow ()
throwFailedWithLog mkException msg = do
  L.logError ("" :: Text) $ msg <> ""
  L.throwException $ mkException $ msg <> ""

data AppException
  = SqlDBConnectionFailedException Text
  | KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
