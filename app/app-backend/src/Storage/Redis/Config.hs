module Storage.Redis.Config where

import EulerHS.Prelude
import qualified EulerHS.Types as T

defaultRedisConfig :: T.RedisConfig
defaultRedisConfig =
  T.RedisConfig
    { connectHost = "127.0.0.1",
      connectPort = 6380,
      connectAuth = Nothing,
      connectDatabase = 0,
      connectMaxConnections = 50,
      connectMaxIdleTime = 30,
      connectTimeout = Nothing
    }
