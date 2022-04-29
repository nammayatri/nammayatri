module Beckn.Utils.Migration
  ( migrateIfNeeded,
  )
where

import Beckn.Prelude
import Beckn.Storage.DB.Config
import Beckn.Storage.Esqueleto.Migration (migrateIfNeeded')
import Beckn.Types.Common
import Control.Exception.Safe
import qualified Database.PostgreSQL.Simple as PS
import qualified EulerHS.Types as T

fromPgConfig :: T.PostgresConfig -> PS.ConnectInfo
fromPgConfig T.PostgresConfig {..} = PS.ConnectInfo {..}

migrateIfNeeded :: (MonadMask m, MonadIO m, Log m) => Maybe FilePath -> Bool -> DBConfig -> m (Either String ())
migrateIfNeeded mPath autoMigrate dbConfig =
  migrateIfNeeded' mPath (const $ pure ()) autoMigrate schemaName connectInfo
  where
    schemaName = encodeUtf8 dbConfig.schemaName
    connectInfo = fromPgConfig dbConfig.pgConfig
