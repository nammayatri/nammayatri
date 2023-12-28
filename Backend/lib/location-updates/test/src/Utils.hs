{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Utils where

import qualified Data.HashMap.Strict as HMS
import qualified EulerHS.Runtime as R
import Kernel.External.Encryption (EncTools (..))
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Hedis.Config
import qualified Kernel.Storage.Hedis.Queries as Hedis
import qualified Kernel.Tools.Metrics.CoreMetrics.Types as Metrics
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Common
import Kernel.Utils.IOLogging
import Kernel.Utils.Servant.SignatureAuth
import qualified "mock-google" Lib.IntegrationTests.Environment as Environment
import Network.HTTP.Client

data Person

-------------------------------------------------
--------------------run types--------------------
data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    encTools :: EncTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions,
    snapToRoadSnippetThreshold :: HighPrecMeters,
    version :: Metrics.DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool
  }
  deriving (Generic)

type TestM = FlowR AppEnv

runFlow :: Text -> AppEnv -> FlowR AppEnv a -> IO a
runFlow tag appEnv flow = do
  liftIO $
    R.withFlowRuntime Nothing $ \flowRt -> do
      flowRt' <-
        runFlowR flowRt appEnv $
          addAuthManagersToFlowRt flowRt [(Just defaultHttpClientOptions.timeoutMs, HMS.singleton "default" defaultManagerSettings)]
      -- FIXME: this is a termporary solution, better fix core code relating to these managers
      runFlowR flowRt' appEnv $ withLogTag tag flow

defaultHttpClientOptions :: HttpClientOptions
defaultHttpClientOptions =
  HttpClientOptions
    { timeoutMs = 2000
    }

wrapTests :: (Environment.AppCfg -> AppEnv -> IO a) -> IO a
wrapTests func = do
  withHedisEnv defaultHedisCfg ("locationUpdatesTest:" <>) $ \hedisEnv -> do
    let loggerConfig = defaultLoggerConfig {logToFile = True, prettyPrinting = True}
    withLoggerEnv loggerConfig Nothing $ \loggerEnv -> do
      coreMetrics <- Metrics.registerCoreMetricsContainer
      -- fetch google configs for using mock-google or real google
      appCfg <- Environment.readConfig "../"
      version <- lookupDeploymentVersion
      let appEnv =
            AppEnv
              { httpClientOptions = defaultHttpClientOptions,
                encTools = appCfg.encTools,
                snapToRoadSnippetThreshold = appCfg.snapToRoadSnippetThreshold,
                ..
              }
      func appCfg appEnv

------------------- utility functions ---------------------

incrDistance :: Id Person -> Double -> TestM Double
incrDistance driverId = Hedis.incrByFloat driverId.getId

updateDistanceTest :: Id Person -> HighPrecMeters -> Int -> Int -> TestM ()
updateDistanceTest driverId dist _ _ = void $ incrDistance driverId (realToFrac dist)

checkTraveledDistance :: Id Person -> TestM Double
checkTraveledDistance driverId = incrDistance driverId 0

deleteDistanceKey :: Id Person -> TestM ()
deleteDistanceKey driverId = Hedis.del driverId.getId

equalsEps :: Double -> Double -> Double -> Bool
equalsEps eps x y = abs (x - y) < eps

----------------- fixtures ---------------------------------

osrmConfig :: MapsServiceConfig
osrmConfig =
  OSRMConfig
    OSRMCfg
      { osrmUrl = fromJust $ parseBaseUrl "localhost:5000",
        radiusDeviation = Just 20
      }
