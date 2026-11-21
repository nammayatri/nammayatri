module SharedLogic.Allocator.Jobs.NudgeRide.CompleteStaleNudgeRide
  ( completeStaleNudgeRideJob,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.DriverPayout (dispatchPayoutToDriver)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.External.LocationTrackingService.Types (LocationTrackingeServiceConfig)
import qualified SharedLogic.NudgeRide as NudgeRide
import qualified SharedLogic.Ride as SRide
import Storage.Beam.Finance ()
import Storage.Beam.Payment ()
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide

completeStaleNudgeRideJob ::
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
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig],
    HasShortDurationRetryCfg r c,
    HasRequestId r
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
    Just ride -> do
      when (ride.status == DRide.INPROGRESS) $ do
        mbBooking <- QBooking.findById ride.bookingId
        case mbBooking of
          Nothing -> logWarning $ "Booking not found for stale NudgeRide: " <> ride.bookingId.getId
          Just booking -> when (booking.bookingType == DBooking.NudgeRide) $ do
            void $ LF.rideEnd rideId 0.0 0.0 booking.providerId (cast driverId) Nothing Nothing Nothing
            QRide.updateStatus rideId DRide.COMPLETED
            QBooking.updateStatus booking.id DBooking.COMPLETED
            triggerNudgeRidePayout booking ride driverId
      Redis.del (NudgeRide.nudgeRideRedisKey driverId)
      pure Complete

triggerNudgeRidePayout ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasField "blackListedJobs" r [Text],
    SchedulerFlow r,
    HasKafkaProducer r,
    Finance.HasActorInfo m r,
    FinanceBeamFlow.BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r
  ) =>
  DBooking.Booking ->
  DRide.Ride ->
  Id DP.Person ->
  m ()
triggerNudgeRidePayout booking ride driverId = do
  mbPayoutCfg <- Redis.safeGet (NudgeRide.nudgeRidePayoutCfgKey ride.id)
  whenJust (mbPayoutCfg :: Maybe NudgeRide.NudgePayoutConfig) $ \cfg -> do
    let traveledKm = realToFrac ride.traveledDistance.getHighPrecMeters / 1000.0 :: Double
        intervals = floor (traveledKm / cfg.intervalDistanceKm) :: Int
        rawAmt = fromIntegral intervals * cfg.payoutAmountPerInterval
        payoutAmt = min booking.estimatedFare rawAmt
    existingPayout <- QPR.findByEntity ride.id.getId (Just DPayment.DEMAND_HOTSPOT_PAYOUT)
    when (isNothing existingPayout && payoutAmt > 0) $ do
      payoutRequestId <- Id <$> generateGUID
      now <- getCurrentTime
      (payoutVpa, netPayoutAmount, mbAdjustVehicleBalance) <- SRide.getPayoutDetailsForRide ride.id payoutAmt
      mbDriverPerson <- QPerson.findById driverId
      phoneNo <- mapM decrypt (mbDriverPerson >>= (.mobileNumber))
      let payoutStatus =
            if isJust payoutVpa
              then DPR.INITIATED
              else DPR.CASH_PENDING
          payoutRequest =
            DPR.PayoutRequest
              { id = payoutRequestId,
                batchId = Nothing,
                entityName = Just DPayment.DEMAND_HOTSPOT_PAYOUT,
                entityId = ride.id.getId,
                entityRefId = Just booking.id.getId,
                beneficiaryId = driverId.getId,
                amount = Just netPayoutAmount,
                status = payoutStatus,
                retryCount = Nothing,
                failureReason = Nothing,
                payoutTransactionId = Nothing,
                cashMarkedById = Nothing,
                cashMarkedByName = Nothing,
                cashMarkedAt = Nothing,
                expectedCreditTime = if payoutStatus == DPR.INITIATED then Just now else Nothing,
                scheduledAt = if payoutStatus == DPR.INITIATED then Just now else Nothing,
                customerVpa = payoutVpa,
                bankName = Nothing,
                bankAccountLast4 = Nothing,
                customerPhone = phoneNo,
                customerEmail = mbDriverPerson >>= (.email),
                customerName = (.firstName) <$> mbDriverPerson,
                remark = Just "Payout for Demand Hotspot Ride",
                orderType = Just "FULFILL_ONLY",
                city = Nothing,
                createdAt = now,
                updatedAt = now,
                merchantId = booking.providerId.getId,
                merchantOperatingCityId = ride.merchantOperatingCityId.getId,
                payoutFee = mbAdjustVehicleBalance,
                coverageFrom = Nothing,
                coverageTo = Nothing,
                payoutType = Nothing,
                ledgerEntryIds = Nothing
              }
      PayoutRequest.createPayoutRequest payoutRequest
      void $ dispatchPayoutToDriver payoutRequest
