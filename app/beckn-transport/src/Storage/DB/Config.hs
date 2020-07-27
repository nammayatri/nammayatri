module Storage.DB.Config where

import Data.Text as T
import EulerHS.Types
import qualified EulerHS.Types as T

dbSchema :: Text
dbSchema = "atlas_transporter"

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
