module Lib.Scheduler.App
  ( runSchedulerService,
  )
where

import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Randomizer
import Kernel.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Kernel.Storage.Hedis (connectHedis)
import qualified Kernel.Tools.Metrics.Init as Metrics
import Kernel.Types.Common (Seconds (..))
import Kernel.Utils.App
import Kernel.Utils.Common (threadDelaySec)
import Kernel.Utils.IOLogging (prepareLoggerEnv)
import Kernel.Utils.Servant.Server
import Kernel.Utils.Shutdown
import Lib.Scheduler.Environment
import Lib.Scheduler.Handler (SchedulerHandle, handler)
import Lib.Scheduler.Metrics
import Servant (Context (EmptyContext))
import System.Exit
import UnliftIO

runSchedulerService ::
  SchedulerConfig ->
  SchedulerHandle t ->
  IO ()
runSchedulerService SchedulerConfig {..} handle_ = do
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  metrics <- setupSchedulerMetrics
  isShuttingDown <- mkShutdown

  let schedulerEnv = SchedulerEnv {..}
  when (tasksPerIteration <= 0) $ do
    hPutStrLn stderr ("tasksPerIteration should be greater than 0" :: Text)
    exitFailure

  Metrics.serve metricsPort
  let serverStartAction = handler handle_
  randSecDelayBeforeStart <- Seconds <$> getRandomInRange (0, loopIntervalSec.getSeconds)
  threadDelaySec randSecDelayBeforeStart -- to make runners start out_of_sync to reduce probability of picking same tasks.
  withAsync (runSchedulerM schedulerEnv serverStartAction) $ \schedulerAction ->
    runServerGeneric
      schedulerEnv
      (Proxy @HealthCheckAPI)
      healthCheck
      identity
      identity
      EmptyContext
      (const identity)
      (\_ -> cancel schedulerAction)
      runSchedulerM

-- TODO: explore what is going on here
-- To which thread do signals come?
