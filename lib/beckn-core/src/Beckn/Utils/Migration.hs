module Beckn.Utils.Migration
  ( migrateIfNeeded,
  )
where

import Beckn.Storage.DB.Config
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T

connect :: T.PostgresConfig -> IO PS.Connection
connect T.PostgresConfig {..} = PS.connect PS.ConnectInfo {..}

migrateIfNeeded :: MonadFlow m => Maybe FilePath -> DBConfig -> Bool -> m (Either String ())
migrateIfNeeded mPath dbCfg autoMigrate =
  case mPath of
    Just path | autoMigrate ->
      fmap resultToEither $ do
        logInfo $ "Running migrations (" <> show path <> ") ..."
        conn <- L.runIO . connect $ pgConfig dbCfg
        L.runIO . PS.withTransaction conn $
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
