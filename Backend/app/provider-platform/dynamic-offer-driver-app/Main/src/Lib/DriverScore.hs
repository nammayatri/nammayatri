{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverScore
  ( driverScoreEventHandler,
  )
where

import Control.Applicative (liftA2)
import Data.Time (utctDay)
import qualified Domain.Types.Common as SRD
import qualified Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverStats as DS
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.VehicleVariant as DVV
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.Environment
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import SharedLogic.BehaviourManagement.IssueBreach (IssueBreachType (..))
import qualified SharedLogic.BehaviourManagement.IssueBreachMitigation as IBM
import qualified SharedLogic.DriverPool as DP
import SharedLogic.External.LocationTrackingService.Types
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as BQ
import qualified Storage.Queries.BookingCancellationReason as BCRQ
import qualified Storage.Queries.DailyStats as SQDS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as DSQ
import qualified Storage.Queries.FareParameters as FPQ
import qualified Storage.Queries.FareParameters.FareParametersProgressiveDetails as FPPDQ
import qualified Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as RQ
import Tools.Constants
import Tools.Error
import Tools.MarketingEvents as TM
import Tools.Metrics (CoreMetrics)
import Utils.Common.Cac.KeyNameConstants

driverScoreEventHandler :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasLocationService m r, EncFlow m r, JobCreator r m, HasFlowEnv m r '["maxNotificationShards" ::: Int], HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DMOC.MerchantOperatingCity -> DST.DriverRideRequest -> m ()
driverScoreEventHandler merchantOpCityId payload = fork "DRIVER_SCORE_EVENT_HANDLER" do
  eventPayloadHandler merchantOpCityId payload

eventPayloadHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasLocationService m r, MonadFlow m, JobCreator r m, HasFlowEnv m r '["maxNotificationShards" ::: Int], HasShortDurationRetryCfg r c, HasKafkaProducer r) => Id DMOC.MerchantOperatingCity -> DST.DriverRideRequest -> m ()
eventPayloadHandler merchantOpCityId DST.OnDriverAcceptingSearchRequest {..} = do
  DP.removeSearchReqIdFromMap merchantId driverId searchReqId
  case response of
    SRD.Accept -> do
      DP.incrementQuoteAcceptedCount merchantOpCityId driverId
      forM_ restDriverIds $ \restDriverId -> do
        DP.decrementTotalQuotesCount merchantId merchantOpCityId (cast restDriverId) searchReqId
        DP.removeSearchReqIdFromMap merchantId restDriverId searchReqId
    SRD.Reject -> pure ()
    SRD.Pulled -> pure ()
eventPayloadHandler merchantOpCityId DST.OnNewRideAssigned {..} = do
  windowSize <- SCR.getWindowSize merchantOpCityId
  void $ SCR.incrementAssignedCount driverId windowSize
  mbDriverStats <- B.runInReplica $ DSQ.findById (cast driverId)
  -- mbDriverStats <- DSQ.findById (cast driverId)
  void $ case mbDriverStats of
    Just driverStats -> incrementOrSetTotalRides driverId driverStats
    Nothing -> createDriverStat currency distanceUnit driverId
  DP.incrementTotalRidesCount merchantOpCityId driverId
eventPayloadHandler merchantOpCityId DST.OnNewSearchRequestForDrivers {..} =
  forM_ driverPool $ \dPoolRes -> DP.incrementTotalQuotesCount searchReq.providerId merchantOpCityId (cast dPoolRes.driverPoolResult.driverId) searchReq validTill batchProcessTime
eventPayloadHandler merchantOpCityId DST.OnDriverCancellation {..} = do
  let driverId = driver.id
  merchantConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (validDriverCancellation `elem` rideTags) $ do
    let windowSize = toInteger $ fromMaybe 7 merchantConfig.cancellationRateWindow
    void $ SCR.incrementCancelledCount driverId windowSize
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  IBM.issueBreachMitigation EXTRA_FARE_MITIGATION merchantConfig driverInfo
  when (doCancellationRateBasedBlocking == Just True && not (driverInfo.onRide)) $
    SCR.nudgeOrBlockDriver merchantConfig driver driverInfo
  mbDriverStats <- B.runInReplica $ DSQ.findById (cast driverId)
  -- mbDriverStats <- DSQ.findById (cast driverId)
  driverStats <- getDriverStats currency distanceUnit mbDriverStats driverId rideFare
  cancellationRateExceeded <- overallCancellationRate driverStats merchantConfig
  when (driverStats.totalRidesAssigned > merchantConfig.minRidesToUnlist && cancellationRateExceeded) $ do
    logDebug $ "Blocking Driver: " <> driverId.getId
    QDI.updateBlockedState (cast driverId) True (Just "AUTOMATICALLY_BLOCKED_DUE_TO_CANCELLATIONS") merchantId merchantOpCityId DTDBT.Application
  DP.incrementCancellationCount merchantOpCityId driverId
  where
    overallCancellationRate driverStats merchantConfig = do
      let rate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int)
          threshold = fromMaybe 65 $ merchantConfig.thresholdCancellationPercentageToUnlist
      logDebug $ "cancellationRate" <> show rate
      pure $ rate > threshold
    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a
eventPayloadHandler merchantOpCityId DST.OnRideCompletion {..} = do
  mbDriverStats <- B.runInReplica $ DSQ.findById (cast driverId) -- always be just because stats will be created at OnNewRideAssigned
  -- mbDriverStats <- DSQ.findById (cast driverId) -- always be just because stats will be created at OnNewRideAssigned
  updateDailyStats driverId merchantOpCityId ride fareParameter
  whenJust mbDriverStats $ \driverStats -> do
    logDebug $ "total rides is :" <> show driverStats.totalRides
    when (driverStats.totalRides == 1) $ do
      driver <- SQP.findById driverId
      whenJust driver $ \driver' -> do
        let vehicleVariants = ride.vehicleVariant
        whenJust vehicleVariants $ \vehicleVariant' -> do
          let vehicleCategory = DVV.castVehicleVariantToVehicleCategory vehicleVariant'
          TM.notifyMarketingEvents driverId driver'.deviceToken (TM.FIRST_RIDE_COMPLETED) (Just vehicleCategory) (MerchantOperatingCityId merchantOpCityId) [TM.FIREBASE]
    (incrementTotalEarningsBy, incrementBonusEarningsBy, incrementLateNightTripsCountBy, overallPickupCharges) <- do
      if isNotBackFilled driverStats
        then do
          allRides <- B.runInReplica $ RQ.findAllRidesByDriverId driverId
          -- allRides <- RQ.findAllRidesByDriverId driverId
          let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
              farePramIds = mapMaybe (.fareParametersId) completedRides
              totalEarnings = sum $ map (fromMaybe 0.0 . (.fare)) completedRides
          driverSelectedFareEarnings <- B.runInReplica $ FPQ.findDriverSelectedFareEarnings farePramIds
          -- driverSelectedFareEarnings <- FPQ.findDriverSelectedFareEarnings farePramIds
          customerExtraFeeEarnings <- B.runInReplica $ FPQ.findCustomerExtraFees farePramIds
          -- customerExtraFeeEarnings <- FPQ.findCustomerExtraFees farePramIds
          let incrementBonusEarningsBy = driverSelectedFareEarnings + customerExtraFeeEarnings
          incrementLateNightTripsCountBy <- B.runInReplica $ FPQ.findAllLateNightRides farePramIds
          -- incrementLateNightTripsCountBy <- FPQ.findAllLateNightRides farePramIds
          deadKmFare <- B.runInReplica $ FPPDQ.findDeadKmFareEarnings farePramIds
          pure (totalEarnings, incrementBonusEarningsBy, incrementLateNightTripsCountBy, deadKmFare)
        else do
          mbBooking <- B.runInReplica $ BQ.findById ride.bookingId
          -- mbBooking <- BQ.findById ride.bookingId
          let incrementBonusEarningsBy = fromMaybe 0.0 $ (\booking' -> Just $ fromMaybe 0.0 booking'.fareParams.driverSelectedFare + fromMaybe 0.0 booking'.fareParams.customerExtraFee) =<< mbBooking
          incrementLateNightTripsCountBy <- isLateNightRide fareParameter
          pure (fromMaybe 0.0 ride.fare, incrementBonusEarningsBy, incrementLateNightTripsCountBy, 10)
    -- Esq.runNoTransaction $ do
    DSQ.incrementTotalEarningsAndBonusEarnedAndLateNightTrip (cast driverId) incrementTotalEarningsBy (incrementBonusEarningsBy + overallPickupCharges) incrementLateNightTripsCountBy

    -- for extra fare mitigation --
    mbMerchantConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
    whenJust mbMerchantConfig $ \merchantConfig -> do
      let ibConfig = IBM.getIssueBreachConfig EXTRA_FARE_MITIGATION merchantConfig
      let allowedSTiers = ibConfig <&> (.ibAllowedServiceTiers)
      let isRideAllowedForCounting = maybe False (\allowedServiceTiers -> null allowedServiceTiers || booking.vehicleServiceTier `elem` allowedServiceTiers) allowedSTiers
      when isRideAllowedForCounting $
        whenJust ibConfig $ \config -> do
          IBM.incrementCompletedBookingCounterForIssueBreach EXTRA_FARE_MITIGATION ride.driverId (toInteger config.ibCountWindowSizeInDays)
      IBM.issueBreachMitigation EXTRA_FARE_MITIGATION merchantConfig driverInfo
  where
    isNotBackFilled :: DS.DriverStats -> Bool
    isNotBackFilled driverStats = driverStats.totalEarnings == 0.0 && driverStats.bonusEarned == 0.0 && driverStats.lateNightTrips == 0

    isLateNightRide :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Maybe FareParameters -> m Int
    isLateNightRide mbFareParam = pure . maybe 0 (const 1) $ (.nightShiftCharge) =<< mbFareParam

updateDailyStats :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> DR.Ride -> Maybe FareParameters -> m ()
updateDailyStats driverId merchantOpCityId ride fareParameter = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let deadKmFares =
        ( \x -> case fareParametersDetails x of
            ProgressiveDetails det -> Just ((deadKmFare :: Fare.FParamsProgressiveDetails -> HighPrecMoney) det)
            SlabDetails _ -> Nothing
            RentalDetails det -> Just ((deadKmFare :: Fare.FParamsRentalDetails -> HighPrecMoney) det)
            InterCityDetails det -> Just (pickupCharge det)
            AmbulanceDetails _ -> Nothing
        )
          =<< fareParameter
      rideDuration = maybe 0 (round . uncurry diffUTCTime) $ liftA2 (,) ride.tripEndTime ride.tripStartTime
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  ds <- SQDS.findByDriverIdAndDate driverId (utctDay localTime)
  case ds of
    Nothing -> do
      id <- generateGUIDText
      now <- getCurrentTime
      let dailyStatsOfDriver' =
            DDS.DailyStats
              { id = id,
                driverId = driverId,
                totalEarnings = fromMaybe 0.0 ride.fare,
                numRides = 1,
                totalDistance = fromMaybe 0 ride.chargeableDistance,
                merchantLocalDate = utctDay localTime,
                currency = ride.currency,
                distanceUnit = ride.distanceUnit,
                activatedValidRides = 0,
                referralEarnings = 0.0,
                referralCounts = 0,
                payoutStatus = DDS.Initialized,
                payoutOrderId = Nothing,
                payoutOrderStatus = Nothing,
                tollCharges = fromMaybe 0.0 ride.tollCharges,
                bonusEarnings = fromMaybe 0.0 (fareParameter >>= (.driverSelectedFare)) + fromMaybe 0.0 (fareParameter >>= (.customerExtraFee)) + fromMaybe 0.0 deadKmFares,
                createdAt = now,
                updatedAt = now,
                cancellationCharges = 0.0,
                tipAmount = 0.0,
                totalRideTime = rideDuration,
                numDriversOnboarded = 0,
                numFleetsOnboarded = 0,
                merchantId = DR.merchantId ride,
                merchantOperatingCityId = Just merchantOpCityId,
                onlineDuration = Nothing
              }
      SQDS.create dailyStatsOfDriver'
    Just dailyStats -> do
      let totalEarnings = dailyStats.totalEarnings + fromMaybe 0.0 ride.fare
          numRides = dailyStats.numRides + 1
          totalDistance = dailyStats.totalDistance + fromMaybe 0 ride.chargeableDistance
          merchantLocalDate = utctDay localTime
          tollCharges = dailyStats.tollCharges + fromMaybe 0.0 ride.tollCharges
          bonusEarnings = dailyStats.bonusEarnings + fromMaybe 0.0 (fareParameter >>= (.driverSelectedFare)) + fromMaybe 0.0 (fareParameter >>= (.customerExtraFee)) + fromMaybe 0.0 deadKmFares
          totalRideDuration = dailyStats.totalRideTime + rideDuration
      SQDS.updateByDriverId totalEarnings numRides totalDistance tollCharges bonusEarnings totalRideDuration driverId merchantLocalDate

createDriverStat :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Currency -> DistanceUnit -> Id DP.Person -> m DS.DriverStats
createDriverStat currency distanceUnit driverId = do
  now <- getCurrentTime
  allRides <- B.runInReplica $ RQ.findAllRidesByDriverId driverId
  -- allRides <- RQ.findAllRidesByDriverId driverId
  let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
      farePramIds = mapMaybe (.fareParametersId) completedRides
  cancelledRidesCount <- B.runInReplica $ BCRQ.findAllCancelledByDriverId driverId
  lateNightTripsCount <- B.runInReplica $ FPQ.findAllLateNightRides farePramIds
  cancelledBookingIdsByDriver <- B.runInReplica $ BCRQ.findAllBookingIdsCancelledByDriverId driverId
  missedEarnings <- B.runInReplica $ BQ.findFareForCancelledBookings cancelledBookingIdsByDriver
  driverSelectedFare <- B.runInReplica $ FPQ.findDriverSelectedFareEarnings farePramIds
  deadKmFare <- B.runInReplica $ FPPDQ.findDeadKmFareEarnings farePramIds
  customerExtraFee <- B.runInReplica $ FPQ.findCustomerExtraFees farePramIds
  let driverStat =
        DS.DriverStats
          { driverId = cast driverId,
            idleSince = now,
            totalRides = length completedRides,
            totalEarnings = sum $ map (fromMaybe 0.0 . (.fare)) completedRides,
            bonusEarned = driverSelectedFare + customerExtraFee + deadKmFare,
            lateNightTrips = lateNightTripsCount,
            earningsMissed = missedEarnings,
            totalDistance = highPrecMetersToMeters . sum $ map (.traveledDistance) allRides,
            ridesCancelled = Just cancelledRidesCount,
            totalRidesAssigned = Just $ length allRides,
            coinCovertedToCashLeft = 0.0,
            totalCoinsConvertedCash = 0.0,
            currency,
            distanceUnit,
            favRiderCount = 0,
            rating = Nothing,
            totalRatings = Just 0,
            totalRatingScore = Just 0,
            isValidRating = Just False,
            totalPayoutEarnings = 0.0,
            totalPayoutAmountPaid = Nothing,
            totalValidActivatedRides = 0,
            totalReferralCounts = 0,
            validDriverCancellationTagCount = 0,
            validCustomerCancellationTagCount = 0,
            validCancellationTagsStatsStartDate = Just now,
            updatedAt = now,
            numDriversOnboarded = 0,
            numFleetsOnboarded = 0,
            safetyPlusEarnings = 0.0,
            safetyPlusRideCount = 0,
            onlineDuration = Seconds 0,
            blacklistCoinEvents = Nothing,
            coinsConvertedToDirectPayoutCash = 0.0
          }
  _ <- DSQ.create driverStat
  pure driverStat

incrementOrSetTotalRides :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> DS.DriverStats -> m DS.DriverStats
incrementOrSetTotalRides driverId driverStats = do
  incrementTotaRidesBy <-
    maybe
      ( do
          allRides <- B.runInReplica $ RQ.findAllRidesByDriverId driverId
          -- allRides <- RQ.findAllRidesByDriverId driverId
          pure $ length allRides
      )
      (\_ -> pure 1)
      driverStats.totalRidesAssigned
  _ <- DSQ.incrementTotalRidesAssigned (cast driverId) incrementTotaRidesBy
  pure $ driverStats {DS.totalRidesAssigned = Just $ fromMaybe 0 driverStats.totalRidesAssigned + incrementTotaRidesBy}

getDriverStats :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Currency -> DistanceUnit -> Maybe DS.DriverStats -> Id DP.Person -> Maybe HighPrecMoney -> m DS.DriverStats
getDriverStats currency distanceUnit Nothing driverId _ = createDriverStat currency distanceUnit driverId
getDriverStats _currency _distanceUnit (Just driverStats) driverId rideFare = do
  updatedTotalRideCount <- getTotalRideCount
  cancelledCount <-
    case driverStats.ridesCancelled of
      Nothing -> B.runInReplica $ BCRQ.findAllCancelledByDriverId driverId
      -- Nothing -> BCRQ.findAllCancelledByDriverId driverId
      Just cancelledCount -> pure $ cancelledCount + 1
  earningMissed <-
    case driverStats.earningsMissed of
      0.0 -> do
        cancelledBookingIdsByDriver <- B.runInReplica $ BCRQ.findAllBookingIdsCancelledByDriverId driverId
        -- cancelledBookingIdsByDriver <- BCRQ.findAllBookingIdsCancelledByDriverId driverId
        B.runInReplica $ BQ.findFareForCancelledBookings cancelledBookingIdsByDriver
      -- BQ.findFareForCancelledBookings cancelledBookingIdsByDriver
      _ -> pure $ driverStats.earningsMissed + fromMaybe 0.0 rideFare
  -- Esq.runNoTransaction $ DSQ.setDriverStats (cast driverId) updatedTotalRideCount cancelledCount earningMissed
  DSQ.setDriverStats (cast driverId) updatedTotalRideCount cancelledCount earningMissed
  pure $ driverStats {DS.ridesCancelled = Just cancelledCount, DS.earningsMissed = earningMissed}
  where
    getTotalRideCount = do
      maybe
        ( do
            allRides <- B.runInReplica $ RQ.findAllRidesByDriverId driverId
            -- allRides <- RQ.findAllRidesByDriverId driverId
            pure $ length allRides
        )
        (\_ -> pure 0)
        driverStats.totalRidesAssigned
