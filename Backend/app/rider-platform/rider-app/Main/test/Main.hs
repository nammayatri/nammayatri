import App
import qualified Data.Text as T
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Runtime (withFlowRuntime)
import qualified FRFS.DirectQR as DirectQR
import Kernel.Beam.Types (KafkaConn (..))
import Kernel.Exit
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import Kernel.Utils.FlowLogging
import System.Environment (lookupEnv)
import System.Environment as Env (setEnv)

main :: IO ()
main = do
  -- setEnv "RIDER_APP_CONFIG_PATH" "../../../../dhall-configs/dev/rider-app.dhall"
  -- appCfg <- readDhallConfigDefault "rider-app"
  -- hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  -- let loggerRt = getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  -- appEnv <- try (buildAppEnv appCfg) >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  -- withFlowRuntime (Just loggerRt) $ \flowRt -> do
  --   runFlowR flowRt appEnv $ do
  --     logInfo "Starting Direct QR tests..."
  --     DirectQR.tests flowRt appEnv
  --     logInfo "Finished Direct QR tests"

  -- -- Let the Logs be flushed
  -- threadDelaySec (Seconds 10)
  pure ()
