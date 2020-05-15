{-# LANGUAGE TypeApplications #-}

module Storage.DB.Config where

import Data.Text as T
import qualified Database.Beam.MySQL as BM
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant.Server
import System.Environment
import Types.Config (Config (..))
import qualified Prelude as P (show)

instance Config T.MySQLConfig where
  theConfig =
    T.MySQLConfig
      { connectHost = "127.0.0.1",
        connectPort = 3306,
        connectUser = "atlas",
        connectPassword = "atlas",
        connectDatabase = "atlas_transporter",
        connectOptions = [T.CharsetName "utf8"],
        connectPath = "",
        connectSSL = Nothing
      }

poolConfig :: T.PoolConfig
poolConfig =
  T.PoolConfig
    { stripes = 1,
      keepAlive = 10,
      resourcesPerStripe = 50
    }

loadMysqlConfig :: IO (Maybe T.MySQLConfig)
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
    Just $
      T.MySQLConfig
        { connectHost = host,
          connectPort = p,
          connectUser = user,
          connectPassword = pass,
          connectDatabase = db,
          connectOptions = [T.CharsetName "utf8"],
          connectPath = "",
          connectSSL = Nothing
        }

getMysqlDBConfig :: T.MySQLConfig -> L.Flow (T.DBConfig BM.MySQLM)
getMysqlDBConfig defMysqlConfig = do
  mConfig <- L.runIO loadMysqlConfig
  case mConfig of
    Nothing -> do
      L.runIO $ putStrLn @String "Could not load mysql config from env. Using defaults."
      pure $ T.mkMySQLPoolConfig (T.pack "transporterDb") defMysqlConfig poolConfig
    Just config -> pure $ T.mkMySQLPoolConfig (T.pack "transporterDb") config poolConfig

-- helper
dbHandle :: (T.DBConfig beM -> L.Flow (Either T.DBError a)) -> T.DBConfig beM -> L.Flow a
dbHandle f cfg = f cfg >>= either (error . show) pure

connMySQLorFail, getConn, getOrInitConn :: T.DBConfig beM -> L.Flow (T.SqlConn beM)
connMySQLorFail = dbHandle L.initSqlDBConnection
getConn = dbHandle L.getSqlDBConnection
getOrInitConn = dbHandle L.getOrInitSqlConn

prepareDBConnections :: Config T.MySQLConfig => L.Flow (T.SqlConn BM.MySQLM)
prepareDBConnections = getMysqlDBConfig theConfig >>= connMySQLorFail
