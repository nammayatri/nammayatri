{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App
  ( runSearchResultAggregator,
  )
where

import qualified Control.Concurrent as CC
import Environment
import Kernel.Exit
import Kernel.Prelude
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServer)
import Servant
import qualified Service.Runner as Runner

type HealthCheckAPI = Get '[JSON] Text

healthCheckServer :: FlowServer HealthCheckAPI
healthCheckServer = pure "App is up"

runSearchResultAggregator :: (AppCfg -> AppCfg) -> IO ()
runSearchResultAggregator configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "search-result-aggregator"
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  runServer appEnv (Proxy @HealthCheckAPI) healthCheckServer identity identity EmptyContext (startService appEnv) releaseAppEnv pure
  where
    startService appEnv flowRt serverStartAction = do
      _ <- CC.forkIO serverStartAction
      runFlowR flowRt appEnv Runner.run
