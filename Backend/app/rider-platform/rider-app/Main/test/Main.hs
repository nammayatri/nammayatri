{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import App
import qualified Data.Text as T
import "rider-app" Environment
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
import qualified SharedCabDegradedSweepTests
import qualified SharedCabDegradedTests
import qualified SharedCabDemandTests
import qualified SharedCabExpiryTests
import qualified SharedCabLockTests
import qualified SharedCabNoShowTests
import qualified SharedCabRefundRetryTests
import qualified SharedCabSeatTests
import qualified SharedCabStopProgressTests
import System.Environment (lookupEnv)
import System.Environment as Env (setEnv)
import Test.Tasty (defaultMain, testGroup)

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
  defaultMain $ testGroup "rider-app" [SharedCabDemandTests.tests, SharedCabExpiryTests.tests, SharedCabStopProgressTests.tests, SharedCabDegradedTests.tests, SharedCabNoShowTests.tests, SharedCabDegradedSweepTests.tests, SharedCabRefundRetryTests.tests, SharedCabSeatTests.tests, SharedCabLockTests.tests]
