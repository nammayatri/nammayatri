module Beckn.Storage.Esqueleto.Migration
  ( migrateIfNeeded,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration

connect :: EsqDBConfig -> IO PS.Connection
connect EsqDBConfig {..} =
  PS.connect
    PS.ConnectInfo
      { connectHost = T.unpack connectHost,
        connectPort,
        connectUser = T.unpack connectUser,
        connectPassword = T.unpack connectPassword,
        connectDatabase = T.unpack connectDatabase
      }

migrateIfNeeded :: (MonadIO m, Log m) => Maybe FilePath -> EsqDBConfig -> Bool -> m (Either String ())
migrateIfNeeded mPath dbCfg autoMigrate =
  case mPath of
    Just path | autoMigrate ->
      fmap resultToEither $ do
        logInfo $ "Running migrations (" <> show path <> ") ..."
        conn <- liftIO $ connect dbCfg
        liftIO . PS.withTransaction conn $
          runMigrations
            True
            conn
            [ MigrationInitialization,
              MigrationDirectory path
            ]
    _ ->
      pure $ Right ()
  where
    resultToEither MigrationSuccess = Right ()
    resultToEither (MigrationError a) = Left a
