module Storage.DB.Config where

import EulerHS.Prelude
import qualified EulerHS.Types as T

dbSchema :: Text
dbSchema = "atlas_gateway"

connectionTag :: T.ConnTag
connectionTag = "gatewayDb"

defaultDbConfig :: T.PostgresConfig
defaultDbConfig =
  T.PostgresConfig
    { connectHost = "127.0.0.1",
      connectPort = 5435,
      connectUser = "atlas",
      connectPassword = "atlas",
      connectDatabase = "atlas_gateway"
    }
