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
import Kernel.Storage.Esqueleto.Logger
import Kernel.Storage.Esqueleto.Migration
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Dhall
import Kernel.Utils.Servant.Server
import Servant
import Tools.Auth

runSpecialZone :: (AppCfg -> AppCfg) -> IO ()
runSpecialZone configModifier = do
  appCfg <- readDhallConfigDefault "special-zone" <&> configModifier
  appEnv <- buildAppEnv appCfg
  let runMigrations :: LoggerIO ()
      runMigrations = do
        eithRes <- migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
        handleLeft exitDBMigrationFailure "Couldn't migrate database: " eithRes
  runLoggerIO appEnv.loggerEnv runMigrations
  runServer appEnv (Proxy @API) handler identity identity context (const identity) releaseAppEnv pure
  where
    context = verifyDashboardAction @(FlowR AppEnv) :. EmptyContext
