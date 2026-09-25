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
import qualified SharedCabAllocationTests
import qualified SharedCabConfigTests
import qualified SharedCabDemandTests
import qualified SharedCabDriverActionTests
import qualified SharedCabEventsTests
import qualified SharedCabExpiryTests
import qualified SharedCabInvariantsTests
import qualified SharedCabLegStateTests
import qualified SharedCabNotifyTests
import qualified SharedCabPlateTests
import qualified SharedCabSessionTests
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
  defaultMain $ testGroup "rider-app" [SharedCabPlateTests.tests, SharedCabSessionTests.tests, SharedCabLegStateTests.tests, SharedCabInvariantsTests.tests, SharedCabNotifyTests.tests, SharedCabDemandTests.tests, SharedCabConfigTests.tests, SharedCabAllocationTests.tests, SharedCabDriverActionTests.tests, SharedCabExpiryTests.tests, SharedCabEventsTests.tests]
