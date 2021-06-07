module Beckn.Storage.Redis.Config where

import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T

prepareRedisConnections :: (L.MonadFlow mFlow, Log mFlow) => T.RedisConfig -> mFlow ()
prepareRedisConnections redisCfg = do
  L.getOrInitKVDBConn (T.mkKVDBConfig "redis" redisCfg) >>= throwOnFailedWithLog
  L.runKVDB "redis" (L.setex "dummy" 1 "dummy") >>= throwOnFailedWithLog
  where
    throwOnFailedWithLog (Left err) = do
      logTagError "" $ errmsg err
      L.throwException $ KVDBConnectionFailedException $ errmsg err
    throwOnFailedWithLog _ = pure ()
    errmsg err = "Failed to get or initialize connection to Redis. " <> show err

newtype AppException
  = KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
