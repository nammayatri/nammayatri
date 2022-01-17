module Types.Environment where

import Beckn.Prelude
import Beckn.Utils.Dhall (FromDhall)

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
    selfUri :: BaseUrl
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> AppEnv
buildAppEnv AppCfg {..} = AppEnv {..}
