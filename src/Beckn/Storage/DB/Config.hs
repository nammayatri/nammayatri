module Beckn.Storage.DB.Config where

import Data.Text as T
import EulerHS.Language
import EulerHS.Language as L
import EulerHS.Prelude hiding (id, show)
import EulerHS.Types
import qualified EulerHS.Types as T
import qualified Prelude as P (show)
import Servant.Server

poolConfig :: PoolConfig
poolConfig = T.PoolConfig {stripes = 1, keepAlive = 10, resourcesPerStripe = 50}

mySQLCfg :: MySQLConfig
mySQLCfg =
  MySQLConfig
    { connectHost = "localhost"
    , connectPort = 3306
    , connectUser = "cloud"
    , connectPassword = "scape"
    , connectDatabase = "jdb"
    , connectOptions = [CharsetName "utf8"]
    , connectPath = ""
    , connectSSL = Nothing
    }

mysqlDBC = mkMySQLPoolConfig "epassMysqlDB" mySQLCfg poolConfig
