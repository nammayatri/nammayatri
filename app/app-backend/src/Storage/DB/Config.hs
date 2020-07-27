module Storage.DB.Config where

import Data.Text as T
import qualified EulerHS.Types as T

dbSchema :: Text
dbSchema = "atlas_app"

connectionTag :: T.ConnTag
connectionTag = "providerDb"

defaultDbConfig :: T.PostgresConfig
defaultDbConfig =
  T.PostgresConfig
    { connectHost = "127.0.0.1",
      connectPort = 5433,
      connectUser = "atlas",
      connectPassword = "atlas",
      connectDatabase = "atlas_app"
    }
