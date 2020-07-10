module Storage.Redis.Config where

import Beckn.Types.Common
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import System.Environment

redisConfig :: T.RedisConfig
redisConfig =
  T.RedisConfig
    { connectHost = "127.0.0.1",
      connectPort = 6380,
      connectAuth = Nothing,
      connectDatabase = 0,
      connectMaxConnections = 50,
      connectMaxIdleTime = 30,
      connectTimeout = Nothing
    }

kvDBConfig :: T.KVDBConfig
kvDBConfig = T.mkKVDBConfig "redis" redisConfig

loadRedisConfig :: IO (Maybe T.KVDBConfig)
loadRedisConfig = do
  mhost <- lookupEnv "REDIS_HOST"
  mport <- lookupEnv "REDIS_PORT"
  mauth <- lookupEnv "REDIS_AUTH"
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
            connectMaxIdleTime = fromRational . toRational . read $ maxIdleTime,
            connectTimeout = fromRational . toRational . read <$> mtimeout
          }

prepareRedisConnections :: Flow ()
prepareRedisConnections = do
  mConfig <- L.runIO loadRedisConfig
  let kvDBConfig' = fromMaybe kvDBConfig mConfig
  kvConn <- L.getOrInitKVDBConn kvDBConfig'
  throwOnFailedWithLog
    kvConn
    KVDBConnectionFailedException
    "Failed to get or initialize connection to Redis."
  eitherResp <- L.runKVDB "redis" $ L.setex "dummy" 1 "dummy"
  case eitherResp of
    Left err ->
      throwFailedWithLog
        KVDBConnectionFailedException
        "Failed to get or initialize connection to Redis."
    Right _ -> pure ()

throwOnFailedWithLog ::
  Show e =>
  Either e a ->
  (Text -> AppException) ->
  Text ->
  Flow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError ("" :: Text) $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
throwOnFailedWithLog _ _ _ = pure ()

throwFailedWithLog :: (Text -> AppException) -> Text -> Flow ()
throwFailedWithLog mkException msg = do
  L.logError ("" :: Text) $ msg <> ""
  L.throwException $ mkException $ msg <> ""

data AppException
  = SqlDBConnectionFailedException Text
  | KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
