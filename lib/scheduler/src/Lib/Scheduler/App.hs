module Lib.Scheduler.App
  ( runSchedulerService,
  )
where

import Beckn.Prelude hiding (mask, throwIO)
import Beckn.Randomizer
import Beckn.Storage.Esqueleto.Config (prepareEsqDBEnv)
import Beckn.Storage.Hedis (connectHedis)
import qualified Beckn.Tools.Metrics.Init as Metrics
import Beckn.Types.Common (Seconds (..))
import Beckn.Utils.App
import Beckn.Utils.Common (threadDelaySec)
import Beckn.Utils.IOLogging (prepareLoggerEnv)
import Beckn.Utils.Servant.Server
import Beckn.Utils.Shutdown
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
