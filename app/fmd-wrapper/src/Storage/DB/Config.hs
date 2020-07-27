module Storage.DB.Config where

import EulerHS.Prelude
import qualified EulerHS.Types as T

dbSchema :: Text
dbSchema = "atlas_fmd_wrapper"

connectionTag :: T.ConnTag
connectionTag = "fmdWrapperDb"

defaultDbConfig :: T.PostgresConfig
defaultDbConfig =
  T.PostgresConfig
    { connectHost = "127.0.0.1",
      connectPort = 5436,
      connectUser = "atlas",
      connectPassword = "atlas",
      connectDatabase = "atlas_fmd_wrapper"
    }
