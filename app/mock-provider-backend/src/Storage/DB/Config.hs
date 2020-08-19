module Storage.DB.Config where

import EulerHS.Prelude
import qualified EulerHS.Types as T

dbSchema :: Text
dbSchema = "atlas_mock_provider_backend"

connectionTag :: T.ConnTag
connectionTag = "mockProviderBackendDb"

defaultDbConfig :: T.PostgresConfig
defaultDbConfig =
  T.PostgresConfig
    { connectHost = "127.0.0.1",
      connectPort = 5438,
      connectUser = "atlas",
      connectPassword = "atlas",
      connectDatabase = "atlas_mock_provider_backend"
    }
