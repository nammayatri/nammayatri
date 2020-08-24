module Storage.DB.Config where

import qualified EulerHS.Types as T

connectionTag :: T.ConnTag
connectionTag = "transporterDb"

defaultDbConfig :: T.PostgresConfig
defaultDbConfig =
  T.PostgresConfig
    { connectHost = "127.0.0.1",
      connectPort = 5434,
      connectUser = "atlas",
      connectPassword = "atlas",
      connectDatabase = "atlas_transporter"
    }
