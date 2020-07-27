module Beckn.Storage.DB.Config where

import Beckn.Types.App
import Database.Beam.Postgres (Pg)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types
import qualified EulerHS.Types as T
import System.Environment

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

getPgDBConfig ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow) =>
  mFlow (T.DBConfig Pg)
getPgDBConfig = do
  commonEnv <- getCommonEnv
  let defPostgresConfig = defaultDbConfig commonEnv
  let tag = connTag commonEnv
  mConfig <- L.runIO loadPgConfig
  case mConfig of
    Nothing -> do
      L.logInfo "Config" "Could not load postgres config from env. Using defaults."
      pure $ T.mkPostgresPoolConfig tag defPostgresConfig poolConfig
    Just config -> pure $ T.mkPostgresPoolConfig tag config poolConfig

-- helper
dbHandle ::
  L.MonadFlow mFlow =>
  (T.DBConfig beM -> mFlow (Either T.DBError a)) ->
  T.DBConfig beM ->
  mFlow a
dbHandle f cfg = f cfg >>= either (error . show) pure

connPgOrFail,
  getConn,
  getOrInitConn ::
    L.MonadFlow mFlow =>
    T.DBConfig bem ->
    mFlow (T.SqlConn bem)
connPgOrFail = dbHandle L.initSqlDBConnection
getConn = dbHandle L.getSqlDBConnection
getOrInitConn = dbHandle L.getOrInitSqlConn

prepareDBConnections ::
  (L.MonadFlow mFlow, HasCommonEnv mFlow) => mFlow (T.SqlConn Pg)
prepareDBConnections = do
  getPgDBConfig >>= connPgOrFail
