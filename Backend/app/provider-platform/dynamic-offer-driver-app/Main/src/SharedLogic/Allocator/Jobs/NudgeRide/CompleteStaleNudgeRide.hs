module SharedLogic.Allocator.Jobs.NudgeRide.CompleteStaleNudgeRide
  ( completeStaleNudgeRideJob,
    endNudgeRide,
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
import SharedLogic.Allocator (AllocatorJobType (..), CompleteStaleNudgeRideJobData (..))
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.External.LocationTrackingService.Types (LocationTrackingeServiceConfig)
import SharedLogic.Finance.PostActions (PostActionsEnv)
import qualified SharedLogic.NudgeRide as NudgeRide
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

completeStaleNudgeRideJob ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    CoreMetrics m,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasField "blackListedJobs" r [Text],
    SchedulerFlow r,
    HasKafkaProducer r,
    Finance.HasActorInfo m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig],
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    PostActionsEnv r
  ) =>
  Job 'CompleteStaleNudgeRide ->
  m ExecutionResult
completeStaleNudgeRideJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      rideId = jobData.rideId
      driverId = jobData.driverId
  mbRide <- QRide.findById rideId
  case mbRide of
    Nothing -> do
      logInfo $ "Ride not found for CompleteStaleNudgeRide job: " <> rideId.getId
      Redis.del (NudgeRide.nudgeRideRedisKey driverId)
      pure Complete
    Just ride | ride.status == DRide.COMPLETED -> do
      logInfo $ "NudgeRide already completed, skipping stale cleanup: " <> rideId.getId
      pure Complete
    Just ride -> do
      mbBooking <- QBooking.findById ride.bookingId
      case (ride.status, mbBooking) of
        (_, Nothing) ->
          logWarning $ "Booking not found for stale NudgeRide: " <> ride.bookingId.getId
        (DRide.NEW, Just booking) | booking.bookingType == DBooking.NudgeRide -> do
          logInfo $ "NudgeRide unanswered, cancelling: " <> rideId.getId
          QRide.updateStatus rideId DRide.CANCELLED
          QBooking.updateStatus booking.id DBooking.CANCELLED
        (DRide.INPROGRESS, Just booking) | booking.bookingType == DBooking.NudgeRide -> do
          endNudgeRide rideId driverId booking ride
        _ -> pure ()
      Redis.del (NudgeRide.nudgeRideRedisKey driverId)
      pure Complete

endNudgeRide ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasField "blackListedJobs" r [Text],
    SchedulerFlow r,
    HasKafkaProducer r,
    Finance.HasActorInfo m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig],
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    CoreMetrics m,
    PostActionsEnv r
  ) =>
  Id DRide.Ride ->
  Id DP.Person ->
  DBooking.Booking ->
  DRide.Ride ->
  m ()
endNudgeRide rideId driverId booking ride = do
  void $ LF.rideEnd rideId 0.0 0.0 booking.providerId (cast driverId) Nothing Nothing Nothing
  QRide.updateStatus rideId DRide.COMPLETED
  QBooking.updateStatus booking.id DBooking.COMPLETED
  NudgeRide.triggerNudgeRidePayout booking ride driverId
