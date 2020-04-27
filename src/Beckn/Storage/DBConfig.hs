module Beckn.Storage.DBConfig where

import           Beckn.Types.Config (Config (..))

import           EulerHS.Prelude
import           EulerHS.Types      (MySQLConfig (..), MySqlOption (..))

instance Config MySQLConfig where
  theConfig = MySQLConfig
    { connectHost     = "127.0.0.1"
    , connectPort     = 3308
    , connectUser     = "cloud"
    , connectPassword = "scape"
    , connectDatabase = "becknDb"
    , connectOptions  = [CharsetName "utf8"]
    , connectPath     = ""
    , connectSSL      = Nothing
    }
