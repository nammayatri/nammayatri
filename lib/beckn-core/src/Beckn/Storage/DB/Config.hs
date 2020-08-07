{-# LANGUAGE TypeApplications #-}

module Beckn.Storage.DB.Config where

import Beckn.Types.App
import qualified Beckn.Types.Storage.ExternalTrail as ExternalTrail
import qualified Beckn.Types.Storage.Trail as Trail
import qualified Database.Beam as B
import Database.Beam.Postgres (Pg)
import qualified Database.Beam.Schema.Tables as B
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
    passw <- mpass
    db <- mdb
    p <- readMaybe port
    Just $
      T.PostgresConfig
        { connectHost = host,
          connectPort = p,
          connectUser = user,
          connectPassword = passw,
          connectDatabase = db
        }

getPgDBConfig ::
  (L.MonadFlow mFlow, HasDbEnv mFlow) =>
  mFlow (T.DBConfig Pg)
getPgDBConfig = do
  dbEnv <- getDbEnv
  let defPostgresConfig = defaultDbConfig dbEnv
  let tag = connTag dbEnv
  mConfig <- L.runIO loadPgConfig
  case mConfig of
    Nothing -> do
      L.logInfo @Text "Config" "Could not load postgres config from env. Using defaults."
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
  (L.MonadFlow mFlow, HasDbEnv mFlow) => mFlow (T.SqlConn Pg)
prepareDBConnections =
  getPgDBConfig >>= connPgOrFail

data TrailDb f = TrailDb
  { _trail :: f (B.TableEntity Trail.TrailT),
    _externalTrail :: f (B.TableEntity ExternalTrail.ExternalTrailT)
  }
  deriving (Generic, B.Database be)

trailDb :: Text -> B.DatabaseSettings be TrailDb
trailDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _trail = setSchema dbSchemaName <> Trail.fieldEMod,
        _externalTrail = setSchema dbSchemaName <> ExternalTrail.fieldEMod
      }
  where
    setSchema x = setEntitySchema (Just x)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
