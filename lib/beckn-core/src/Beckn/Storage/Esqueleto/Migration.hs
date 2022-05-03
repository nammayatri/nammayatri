module Beckn.Storage.Esqueleto.Migration
  ( migrateIfNeeded,
    migrateIfNeeded',
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.Common
import Control.Exception.Safe
import Data.ByteString
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration

fromEsqDBConfig :: EsqDBConfig -> PS.ConnectInfo
fromEsqDBConfig EsqDBConfig {..} =
  PS.ConnectInfo
    { connectHost = T.unpack connectHost,
      connectPort,
      connectUser = T.unpack connectUser,
      connectPassword = T.unpack connectPassword,
      connectDatabase = T.unpack connectDatabase
    }

migrateIfNeeded :: (MonadMask m, MonadIO m, Log m) => Maybe FilePath -> Bool -> EsqDBConfig -> m (Either String ())
migrateIfNeeded mPath autoMigrate esqDbConfig =
  migrateIfNeeded' mPath autoMigrate schemaName connectInfo
  where
    schemaName = encodeUtf8 esqDbConfig.connectSchemaName
    connectInfo = fromEsqDBConfig esqDbConfig

migrateIfNeeded' :: (MonadMask m, MonadIO m, Log m) => Maybe FilePath -> Bool -> ByteString -> PS.ConnectInfo -> m (Either String ())
migrateIfNeeded' mPath autoMigrate schemaName connectInfo =
  case mPath of
    Just path
      | autoMigrate ->
        bracket
          (liftIO (PS.connect connectInfo))
          (liftIO . PS.close)
          (migrate path)
    _ ->
      pure $ Right ()
  where
    options =
      defaultOptions
        { optTableName = schemaName <> "." <> "schema_migrations",
          optVerbose = Verbose
        }
    resultToEither MigrationSuccess = Right ()
    resultToEither (MigrationError a) = Left a
    migrate path conn =
      fmap resultToEither $ do
        logInfo $ "Running migrations (" <> show path <> ") ..."
        liftIO $
          runMigrations
            conn
            options
            [ MigrationInitialization,
              MigrationDirectory path
            ]
