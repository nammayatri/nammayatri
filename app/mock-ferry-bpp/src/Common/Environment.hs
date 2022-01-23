module Common.Environment where

import Beckn.Prelude
import Beckn.Utils.Dhall (FromDhall)
import qualified Database.Redis as Redis

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    redisConnection :: Redis.Connection
  }
  deriving (Generic)

buildAppEnv :: Redis.Connection -> AppCfg -> AppEnv
buildAppEnv redisConnection AppCfg {..} = AppEnv {..}
