module Storage.DB.Config where

import qualified EulerHS.Types as T

connectionTag :: T.ConnTag
connectionTag = "mockAppBackendDb"

defaultDbConfig :: T.PostgresConfig
defaultDbConfig =
  T.PostgresConfig
    { connectHost = "127.0.0.1",
      connectPort = 5437,
      connectUser = "atlas",
      connectPassword = "atlas",
      connectDatabase = "atlas_mock_app_backend"
    }
