{-# LANGUAGE TypeApplications #-}

module Storage.DB.Config where

import App.Types
import Data.Text as T
import Database.Beam.Postgres (Pg)
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import System.Environment

dbSchema :: Text
dbSchema = "atlas_gateway"

class Config a where
  theConfig :: a

instance Config ET.PostgresConfig where
  theConfig =
    ET.PostgresConfig
      { connectHost = "127.0.0.1",
        connectPort = 5435,
        connectUser = "atlas",
        connectPassword = "atlas",
        connectDatabase = "atlas_gateway"
      }

poolConfig :: ET.PoolConfig
poolConfig =
  ET.PoolConfig
    { stripes = 1,
      keepAlive = 10,
      resourcesPerStripe = 50
    }

loadPgConfig :: IO (Maybe ET.PostgresConfig)
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
    dbpass <- mpass
    db <- mdb
    p <- readMaybe port
    Just $
      ET.PostgresConfig
        { connectHost = host,
          connectPort = p,
          connectUser = user,
          connectPassword = dbpass,
          connectDatabase = db
        }

getPgDBConfig :: ET.PostgresConfig -> Flow (ET.DBConfig Pg)
getPgDBConfig defPostgresConfig = do
  mConfig <- EL.runIO loadPgConfig
  case mConfig of
    Nothing -> do
      EL.logInfo @Text "Config" "Could not load postgres config from env. Using defaults."
      pure $ ET.mkPostgresPoolConfig (T.pack "providerDb") defPostgresConfig poolConfig
    Just config -> pure $ ET.mkPostgresPoolConfig (T.pack "providerDb") config poolConfig

-- helper
dbHandle :: (ET.DBConfig beM -> EL.Flow (Either ET.DBError a)) -> ET.DBConfig beM -> Flow a
dbHandle f cfg = lift (f cfg) >>= either (error . show) pure

connPostgresOrFail, getConn, getOrInitConn :: ET.DBConfig beM -> Flow (ET.SqlConn beM)
connPostgresOrFail = dbHandle EL.initSqlDBConnection
getConn = dbHandle EL.getSqlDBConnection
getOrInitConn = dbHandle EL.getOrInitSqlConn

prepareDBConnections :: Config ET.PostgresConfig => Flow (ET.SqlConn Pg)
prepareDBConnections = getPgDBConfig theConfig >>= connPostgresOrFail
