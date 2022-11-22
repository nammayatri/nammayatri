module Types where

import Beckn.External.Encryption (EncTools (..))
import Beckn.Prelude
import Beckn.Storage.Hedis.Config
import qualified Beckn.Storage.Hedis.Queries as Hedis
import qualified Beckn.Tools.Metrics.CoreMetrics.Types as Metrics
import Beckn.Types.Flow
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map as Map
import qualified EulerHS.Runtime as R
import Network.HTTP.Client

data Person

-------------------------------------------------
--------------------run types--------------------
data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    encTools :: EncTools,
    hedisEnv :: HedisEnv,
    coreMetrics :: Metrics.CoreMetricsContainer,
    httpClientOptions :: HttpClientOptions
  }
  deriving (Generic)

type TestM = FlowR AppEnv

runFlow :: Text -> AppEnv -> FlowR AppEnv a -> IO a
runFlow tag appEnv flow = do
  liftIO $
    R.withFlowRuntime Nothing $ \flowRt -> do
      flowRt' <-
        runFlowR flowRt appEnv $
          addAuthManagersToFlowRt flowRt [(Just defaultHttpClientOptions.timeoutMs, Map.singleton "default" defaultManagerSettings)]
      -- FIXME: this is a termporary solution, better fix core code relating to these managers
      runFlowR flowRt' appEnv $ withLogTag tag flow

defaultHttpClientOptions :: HttpClientOptions
defaultHttpClientOptions =
  HttpClientOptions
    { timeoutMs = 2000,
      maxRetries = 3
    }

wrapTests :: (AppEnv -> IO a) -> IO a
wrapTests func = do
  withHedisEnv defaultHedisCfg ("locationUpdatesTest:" <>) $ \hedisEnv -> do
    let loggerConfig = defaultLoggerConfig {logToFile = True, prettyPrinting = True}
    let encTools =
          EncTools
            { service = ("localhost", 8021),
              hashSalt = "How wonderful it is that nobody need wait a single moment before starting to improve the world"
            }
    withLoggerEnv loggerConfig Nothing $ \loggerEnv -> do
      coreMetrics <- Metrics.registerCoreMetricsContainer
      let appEnv = AppEnv loggerConfig loggerEnv encTools hedisEnv coreMetrics defaultHttpClientOptions
      func appEnv

------------------- utility functions ---------------------

incrDistance :: Id Person -> Double -> TestM Double
incrDistance driverId = Hedis.incrByFloat driverId.getId

updateDistanceTest :: Id Person -> HighPrecMeters -> TestM ()
updateDistanceTest driverId dist = void $ incrDistance driverId (realToFrac dist)

checkTraveledDistance :: Id Person -> TestM Double
checkTraveledDistance driverId = incrDistance driverId 0

deleteDistanceKey :: Id Person -> TestM ()
deleteDistanceKey driverId = Hedis.del driverId.getId

equalsEps :: Double -> Double -> Double -> Bool
equalsEps eps x y = abs (x - y) < eps
