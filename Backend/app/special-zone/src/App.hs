{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}

module App
  ( runSpecialZone,
  )
where

import API.Handler
import API.Types
import Environment
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Esqueleto.Logger
import Kernel.Storage.Esqueleto.Migration
import Kernel.Types.Logging
import Kernel.Utils.App
import Kernel.Utils.Servant.Server
import Servant

runSpecialZone :: (AppCfg -> AppCfg) -> IO ()
runSpecialZone configModifier = do
  let config = configModifier defaultConfig
  appEnv <- buildAppEnv config
  let runMigrations :: LoggerIO ()
      runMigrations = do
        eithRes <- migrateIfNeeded config.migrationPath config.autoMigrate config.esqDBCfg
        handleLeft exitDBMigrationFailure "Couldn't migrate database: " eithRes
  runLoggerIO appEnv.loggerEnv runMigrations
  runServer appEnv (Proxy @API) handler identity identity EmptyContext (const identity) releaseAppEnv pure

defaultConfig :: AppCfg -- move to dhall if req'd
defaultConfig =
  AppCfg
    { port = 4747,
      migrationPath = Just "dev/migrations/special-zone",
      autoMigrate = True,
      esqDBCfg =
        EsqDBConfig
          { connectHost = "localhost",
            connectPort = 5434,
            connectUser = "atlas_special_zone_user",
            connectPassword = "atlas",
            connectDatabase = "atlas_dev",
            connectSchemaName = "atlas_special_zone"
          },
      loggerConfig =
        LoggerConfig
          { level = DEBUG,
            logToFile = True,
            logFilePath = "/tmp/special-location.log",
            logToConsole = True,
            logRawSql = True,
            prettyPrinting = True
          },
      graceTerminationPeriod = 90
    }
