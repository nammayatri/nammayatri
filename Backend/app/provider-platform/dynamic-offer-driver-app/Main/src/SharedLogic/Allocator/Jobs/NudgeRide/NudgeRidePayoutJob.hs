module SharedLogic.Allocator.Jobs.NudgeRide.NudgeRidePayoutJob
  ( nudgeRidePayoutJob,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.NudgeRide.CompleteStaleNudgeRide (triggerNudgeRidePayout)
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

nudgeRidePayoutJob ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasField "blackListedJobs" r [Text],
    SchedulerFlow r,
    HasKafkaProducer r,
    Finance.HasActorInfo m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r
  ) =>
  Job 'NudgeRidePayout ->
  m ExecutionResult
nudgeRidePayoutJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      rideId = jobData.rideId
      driverId = jobData.driverId
  mbRide <- QRide.findById rideId
  case mbRide of
    Nothing -> do
      logWarning $ "NudgeRidePayout: ride not found: " <> rideId.getId
      pure Complete
    Just ride -> do
      mbBooking <- QBooking.findById ride.bookingId
      case mbBooking of
        Nothing -> do
          logWarning $ "NudgeRidePayout: booking not found: " <> ride.bookingId.getId
          pure Complete
        Just booking | booking.bookingType == DBooking.NudgeRide -> do
          triggerNudgeRidePayout booking ride driverId
          pure Complete
        _ -> do
          logWarning $ "NudgeRidePayout: not a NudgeRide booking: " <> ride.bookingId.getId
          pure Complete
