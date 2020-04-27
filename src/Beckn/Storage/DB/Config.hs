
module Beckn.Storage.DB.Config where

import           Beckn.Types.Config     (Config (..))

import qualified Data.Text              as Text
import qualified Database.Beam.MySQL    as BM
import qualified EulerHS.Language       as L
import           EulerHS.Prelude
import           EulerHS.Types          hiding (error)
import qualified EulerHS.Types          as T
import           System.Environment


import           Beckn.Storage.DBConfig as DB
import           Data.Text              as T
import           EulerHS.Language
import           EulerHS.Language       as L
import           EulerHS.Prelude        hiding (id, show)
import           EulerHS.Types
import qualified EulerHS.Types          as T
import qualified Prelude                as P (show)
import           Servant.Server


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



poolConfig :: T.PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

loadMysqlConfig :: IO (Maybe MySQLConfig)
loadMysqlConfig = do
  mhost <- lookupEnv "MYSQL_HOST"
  mport <- lookupEnv "MYSQL_PORT"
  muser <- lookupEnv "MYSQL_USER"
  mpass <- lookupEnv "MYSQL_PASSWORD"
  mdb <- lookupEnv "MYSQL_DB"
  pure $ do
    host <- mhost
    port <- mport
    user <- muser
    pass <- mpass
    db <- mdb
    p <- readMaybe port
    Just $ MySQLConfig
      { connectHost     = host
      , connectPort     = p
      , connectUser     = user
      , connectPassword = pass
      , connectDatabase = db
      , connectOptions  = [T.CharsetName "utf8"]
      , connectPath     = ""
      , connectSSL      = Nothing
      }

getMysqlDBConfig :: MySQLConfig -> L.Flow (T.DBConfig BM.MySQLM)
getMysqlDBConfig defMysqlConfig = do
  mConfig <- L.runIO loadMysqlConfig
  case mConfig of
    Nothing -> do
      L.runIO $ putStrLn @String "Could not load mysql config from env. Using defaults."
      pure $ T.mkMySQLPoolConfig (Text.pack "routeMysqlDB") defMysqlConfig poolConfig
    Just config -> pure $ T.mkMySQLPoolConfig (Text.pack "routeMysqlDB") config poolConfig

-- helper
dbHandle :: (T.DBConfig beM -> L.Flow (Either DBError a)) -> T.DBConfig beM -> L.Flow a
dbHandle f cfg = f cfg >>= either (error . show) pure

connMySQLorFail, getConn, getOrInitConn :: T.DBConfig beM -> L.Flow (T.SqlConn beM)
connMySQLorFail = dbHandle L.initSqlDBConnection
getConn         = dbHandle L.getSqlDBConnection
getOrInitConn   = dbHandle L.getOrInitSqlConn

prepareDBConnections :: Config MySQLConfig => L.Flow (T.SqlConn BM.MySQLM)
prepareDBConnections = getMysqlDBConfig mySQLCfg >>= connMySQLorFail
