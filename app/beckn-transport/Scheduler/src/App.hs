module App where

import Beckn.Mock.App (runMock)
import Beckn.Prelude
import Beckn.Scheduler
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBEnv, HasEsqEnv, prepareEsqDBEnv)
import Beckn.Types.Id (Id (Id))
import Beckn.Utils.Common
import Beckn.Utils.Dhall
import Beckn.Utils.IOLogging (LoggerEnv, prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import Data.String.Conversions (cs)
import qualified Domain.Types.RideRequest as RideRequest
import SharedLogic.Schedule
import qualified Storage.Queries.RideRequest as RideRequest
import System.Environment (lookupEnv)

runTransporterScheduler ::
  (SchedulerConfig JobType -> SchedulerConfig JobType) ->
  IO ()
runTransporterScheduler configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "transporter-scheduler"
  hostname <- fmap cs <$> lookupEnv "POD_NAME" :: IO (Maybe Text)
  loggerEnv <- prepareLoggerEnv appCfg.loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv appCfg.esqDBCfg loggerEnv
  let loggerConfig = appCfg.loggerConfig
  let handlerEnv = HandlerEnv {..}
  runScheduler appCfg $ schedulerHandlerList handlerEnv

schedulerHandlerList :: HandlerEnv -> JobHandlerList JobType
schedulerHandlerList env =
  [ (AllocateRental, JobHandler $ \x -> runMock env $ allocateRentalRide x)
  ]

data HandlerEnv = HandlerEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }

--------------------------------------

allocateRentalRide ::
  (HasEsqEnv m r, Log m, HasPrettyLogger m r, C.MonadCatch m, MonadGuid m) =>
  Job JobType AllocateRentalJobData ->
  m ExecutionResult
allocateRentalRide job = C.handleAll (const $ pure Retry) $
  withLogTag ("JobId=" <> job.id.getId) $ do
    guid <- Id <$> generateGUIDText
    now <- getCurrentTime
    let rideReq =
          RideRequest.RideRequest
            { id = guid,
              createdAt = now,
              bookingId = job.jobData.bookingId,
              subscriberId = job.jobData.shortOrgId,
              _type = RideRequest.ALLOCATION,
              info = Nothing
            }
    logInfo $ "allocating rental ride for rideReqestId=" <> job.jobData.bookingId.getId
    logPretty DEBUG "ride request" rideReq
    Esq.runTransaction $ RideRequest.create rideReq
    pure Complete
