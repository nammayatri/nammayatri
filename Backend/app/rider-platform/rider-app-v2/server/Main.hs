{-
  Rider-app-v2 entry point.

  Starts on port 8113, reuses rider-app's AppEnv (same DB, Redis, config).
  Only rider-facing APIs. Beckn callbacks stay on original rider-app (8013).
-}
module Main where

import qualified Data.Text as T
import Environment
import EulerHS.Interpreters (runFlow)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionRider)
import Kernel.Beam.Connection.Types (ConnectionConfigRider (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Exit
import Kernel.Storage.Queries.SystemConfigs
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Servant
import Storage.Beam.SystemConfigs ()
import System.Environment (lookupEnv)
import Wiring.Server (V2API, v2Server)

v2Port :: Int
v2Port = 8113

main :: IO ()
main = do
  appCfg <- readDhallConfigDefault "rider-app"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "
  let settings = defaultSettings & setPort v2Port
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow flowRt $
      prepareConnectionRider
        ( ConnectionConfigRider
            { esqDBCfg = appCfg.esqDBCfg,
              esqDBReplicaCfg = appCfg.esqDBReplicaCfg,
              hedisClusterCfg = appCfg.hedisClusterCfg,
              hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
            }
        )
        appCfg.kvConfigUpdateFrequency
    runFlow flowRt $ do
      systemConfigs <- L.runIO $ readSystemConfigs appCfg.kvConfigUpdateFrequency
      KBT.updateConfig systemConfigs
    putStrLn @String $ "rider-app-v2 starting on port " <> show v2Port
    runSettings settings $
      serve (Proxy @V2API) $
        hoistServer (Proxy @V2API) (transform appEnv flowRt) v2Server
  where
    transform appEnv flowRt handler =
      Servant.Handler . ExceptT . try $
        runFlowR flowRt appEnv handler
