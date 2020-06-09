{-# LANGUAGE TypeApplications #-}

module Storage.DB.Config where

import Data.Text as T
import Database.Beam.Postgres (Pg)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types
import qualified EulerHS.Types as T
import Servant.Server
import System.Environment
import Types.Config (Config (..))
import qualified Prelude as P (show)

dbSchema :: Text
dbSchema = "atlas_transporter"

instance Config T.PostgresConfig where
  theConfig =
    T.PostgresConfig
      { connectHost = "127.0.0.1",
        connectPort = 5434,
        connectUser = "atlas",
        connectPassword = "atlas",
        connectDatabase = "atlas_transporter"
      }

poolConfig :: T.PoolConfig
poolConfig =
  T.PoolConfig
    { stripes = 1,
      keepAlive = 10,
      resourcesPerStripe = 50
    }

loadPgConfig :: IO (Maybe T.PostgresConfig)
loadPgConfig = do
  mhost <- lookupEnv "DB_HOST"
  mport <- lookupEnv "DB_PORT"
  muser <- lookupEnv "DB_USER"
  mpass <- lookupEnv "DB_PASSWORD"
  mdb <- lookupEnv "DB_NAME"
  pure $ do
    host <- mhost
    port <- mport
    user <- muser
    pass <- mpass
    db <- mdb
    p <- readMaybe port
    Just $
      T.PostgresConfig
        { connectHost = host,
          connectPort = p,
          connectUser = user,
          connectPassword = pass,
          connectDatabase = db
        }

getPgDBConfig :: T.PostgresConfig -> L.Flow (T.DBConfig Pg)
getPgDBConfig defPostgresConfig = do
  mConfig <- L.runIO loadPgConfig
  case mConfig of
    Nothing -> do
      L.runIO $ putStrLn @String "Could not load postgres config from env. Using defaults."
      pure $ T.mkPostgresPoolConfig (T.pack "transporterDb") defPostgresConfig poolConfig
    Just config -> pure $ T.mkPostgresPoolConfig (T.pack "transporterDb") config poolConfig

-- helper
dbHandle :: (T.DBConfig beM -> L.Flow (Either T.DBError a)) -> T.DBConfig beM -> L.Flow a
dbHandle f cfg = f cfg >>= either (error . show) pure

connPgOrFail, getConn, getOrInitConn :: T.DBConfig bem -> L.Flow (T.SqlConn bem)
connPgOrFail = dbHandle L.initSqlDBConnection
getConn = dbHandle L.getSqlDBConnection
getOrInitConn = dbHandle L.getOrInitSqlConn

prepareDBConnections :: Config T.PostgresConfig => L.Flow (T.SqlConn Pg)
prepareDBConnections = getPgDBConfig theConfig >>= connPgOrFail
