{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

module Lib.DriverScore
  ( driverScoreEventHandler,
  )
where

import qualified Domain.Types.DriverStats as DS
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequestForDriver as SRD
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common (Forkable (fork), Money (Money), fromMaybeM, getCurrentTime, getMoney, highPrecMetersToMeters, logDebug)
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.DriverPool as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CTCQ
import qualified Storage.Queries.Booking as BQ
import qualified Storage.Queries.BookingCancellationReason as BCRQ
import qualified Storage.Queries.DriverStats as DSQ
import qualified Storage.Queries.FareParameters as FPQ
import qualified Storage.Queries.Ride as RQ
import Tools.Error

driverScoreEventHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DST.DriverRideRequest -> m ()
driverScoreEventHandler payload = fork "DRIVER_SCORE_EVENT_HANDLER" do
  logDebug $ "driverScoreEventHandler with payload: " <> show payload
  eventPayloadHandler payload

eventPayloadHandler :: (Redis.HedisFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => DST.DriverRideRequest -> m ()
eventPayloadHandler DST.OnDriverAcceptingSearchRequest {..} = do
  DP.removeSearchReqIdFromMap merchantId driverId searchTryId
  case response of
    SRD.Accept -> do
      DP.incrementQuoteAcceptedCount merchantId driverId
      forM_ restDriverIds $ \restDriverId -> do
        DP.decrementTotalQuotesCount merchantId (cast restDriverId) searchTryId
        DP.removeSearchReqIdFromMap merchantId restDriverId searchTryId
    SRD.Reject -> pure ()
    SRD.Pulled -> pure ()
eventPayloadHandler DST.OnNewRideAssigned {..} = do
  -- mbDriverStats <- Esq.runInReplica $ DSQ.findById (cast driverId)
  mbDriverStats <- DSQ.findById (cast driverId)
  void $ case mbDriverStats of
    Just driverStats -> incrementOrSetTotaRides driverId driverStats
    Nothing -> createDriverStat driverId
  DP.incrementTotalRidesCount merchantId driverId
eventPayloadHandler DST.OnNewSearchRequestForDrivers {..} =
  forM_ driverPool $ \dPoolRes -> DP.incrementTotalQuotesCount searchReq.providerId (cast dPoolRes.driverPoolResult.driverId) searchReq validTill batchProcessTime
eventPayloadHandler DST.OnDriverCancellation {..} = do
  merchantConfig <- CTCQ.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  -- mbDriverStats <- Esq.runInReplica $ DSQ.findById (cast driverId)
  mbDriverStats <- DSQ.findById (cast driverId)
  driverStats <- getDriverStats mbDriverStats driverId rideFare
  cancellationRateExcedded <- overallCancellationRate driverStats merchantConfig
  when (driverStats.totalRidesAssigned > merchantConfig.minRidesToUnlist && cancellationRateExcedded) $ do
    logDebug $ "Blocking Driver: " <> driverId.getId
    void $ CDI.updateBlockedState (cast driverId) True
  DP.incrementCancellationCount merchantId driverId
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
eventPayloadHandler DST.OnRideCompletion {..} = do
  -- mbDriverStats <- Esq.runInReplica $ DSQ.findById (cast driverId) -- always be just because stats will be created at OnNewRideAssigned
  mbDriverStats <- DSQ.findById (cast driverId) -- always be just because stats will be created at OnNewRideAssigned
  whenJust mbDriverStats $ \driverStats -> do
    (incrementTotaEarningsBy, incrementBonusEarningsBy, incrementLateNightTripsCountBy, overallPickupCharges) <-
      if isNotBackFilled driverStats
        then do
          -- allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
          allRides <- RQ.findAllRidesByDriverId driverId
          let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
              farePramIds = mapMaybe (.fareParametersId) completedRides
              totalEarnings = sum $ map (fromMaybe 0 . (.fare)) completedRides
          -- driverSelectedFareEarnings <- Esq.runInReplica $ FPQ.findDriverSelectedFareEarnings farePramIds
          driverSelectedFareEarnings <- FPQ.findDriverSelectedFareEarnings farePramIds
          -- customerExtraFeeEarnings <- Esq.runInReplica $ FPQ.findCustomerExtraFees farePramIds
          customerExtraFeeEarnings <- FPQ.findCustomerExtraFees farePramIds
          let incrementBonusEarningsBy = driverSelectedFareEarnings + customerExtraFeeEarnings
          -- incrementLateNightTripsCountBy <- Esq.runInReplica $ FPQ.findAllLateNightRides farePramIds
          incrementLateNightTripsCountBy <- FPQ.findAllLateNightRides farePramIds
          pure (totalEarnings, incrementBonusEarningsBy, incrementLateNightTripsCountBy, Money (length farePramIds * 10))
        else do
          -- mbBooking <- Esq.runInReplica $ BQ.findById ride.bookingId
          mbBooking <- BQ.findById ride.bookingId
          let incrementBonusEarningsBy = fromMaybe 0 $ (\booking -> Just $ fromMaybe 0 (getMoney <$> booking.fareParams.driverSelectedFare) + fromMaybe 0 (getMoney <$> booking.fareParams.customerExtraFee)) =<< mbBooking
          incrementLateNightTripsCountBy <- isLateNightRide ride
          pure (fromMaybe (Money 0) ride.fare, incrementBonusEarningsBy, incrementLateNightTripsCountBy, 10)
    -- Esq.runNoTransaction $ do
    DSQ.incrementTotalEarningsAndBonusEarnedAndLateNightTrip (cast driverId) incrementTotaEarningsBy (Money incrementBonusEarningsBy + overallPickupCharges) incrementLateNightTripsCountBy
  where
    isNotBackFilled :: DS.DriverStats -> Bool
    isNotBackFilled driverStats = driverStats.totalEarnings == 0 && driverStats.bonusEarned == 0 && driverStats.lateNightTrips == 0 && driverStats.earningsMissed == 0

    isLateNightRide :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => DR.Ride -> m Int
    isLateNightRide rd = do
      case rd.fareParametersId of
        Just fareParamId -> do
          -- mbFareParam <- Esq.runInReplica $ FPQ.findById fareParamId
          mbFareParam <- FPQ.findById fareParamId
          pure . maybe 0 (const 1) $ (.nightShiftCharge) =<< mbFareParam
        Nothing -> pure 0

createDriverStat :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> m DS.DriverStats
createDriverStat driverId = do
  now <- getCurrentTime
  -- allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
  allRides <- RQ.findAllRidesByDriverId driverId
  let completedRides = filter ((== DR.COMPLETED) . (.status)) allRides
      farePramIds = mapMaybe (.fareParametersId) completedRides
  -- cancelledRidesCount <- Esq.runInReplica $ BCRQ.findAllCancelledByDriverId driverId
  -- lateNightTripsCount <- Esq.runInReplica $ FPQ.findAllLateNightRides farePramIds
  -- cancelledBookingIds <- Esq.runInReplica $ RQ.findCancelledBookingId driverId
  cancelledBookingIds <- RQ.findCancelledBookingId driverId
  -- missedEarnings <- Esq.runInReplica $ BQ.findFareForCancelledBookings cancelledBookingIds
  -- driverSelectedFare <- Esq.runInReplica $ FPQ.findDriverSelectedFareEarnings farePramIds
  -- customerExtraFee <- Esq.runInReplica $ FPQ.findCustomerExtraFees farePramIds
  cancelledRidesCount <- BCRQ.findAllCancelledByDriverId driverId
  lateNightTripsCount <- FPQ.findAllLateNightRides farePramIds
  missedEarnings <- BQ.findFareForCancelledBookings cancelledBookingIds
  driverSelectedFare <- FPQ.findDriverSelectedFareEarnings farePramIds
  customerExtraFee <- FPQ.findCustomerExtraFees farePramIds
  let driverStat =
        DS.DriverStats
          { driverId = cast driverId,
            idleSince = now,
            totalRides = length completedRides,
            totalEarnings = sum $ map (fromMaybe 0 . (.fare)) completedRides,
            bonusEarned = Money (driverSelectedFare + customerExtraFee + length farePramIds * 10),
            lateNightTrips = lateNightTripsCount,
            earningsMissed = missedEarnings,
            totalDistance = highPrecMetersToMeters . sum $ map (.traveledDistance) allRides,
            ridesCancelled = Just cancelledRidesCount,
            totalRidesAssigned = Just $ length allRides,
            updatedAt = now
          }
  _ <- DSQ.create driverStat
  pure driverStat

incrementOrSetTotaRides :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DP.Person -> DS.DriverStats -> m DS.DriverStats
incrementOrSetTotaRides driverId driverStats = do
  incrementTotaRidesBy <-
    maybe
      ( do
          -- allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
          allRides <- RQ.findAllRidesByDriverId driverId
          pure $ length allRides
      )
      (\_ -> pure 1)
      driverStats.totalRidesAssigned
  _ <- DSQ.incrementTotalRidesAssigned (cast driverId) incrementTotaRidesBy
  pure $ driverStats {DS.totalRidesAssigned = Just $ fromMaybe 0 driverStats.totalRidesAssigned + incrementTotaRidesBy}

getDriverStats :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Maybe DS.DriverStats -> Id DP.Person -> Maybe Money -> m DS.DriverStats
getDriverStats Nothing driverId _ = createDriverStat driverId
getDriverStats (Just driverStats) driverId rideFare = do
  updatedTotalRideCount <- getTotalRideCount
  cancelledCount <-
    case driverStats.ridesCancelled of
      -- Nothing -> Esq.runInReplica $ BCRQ.findAllCancelledByDriverId driverId
      Nothing -> BCRQ.findAllCancelledByDriverId driverId
      Just cancelledCount -> pure $ cancelledCount + 1
  earningMissed <-
    case driverStats.earningsMissed of
      0 -> do
        -- cancelledBookingIds <- Esq.runInReplica $ RQ.findCancelledBookingId driverId
        cancelledBookingIds <- RQ.findCancelledBookingId driverId
        -- Esq.runInReplica $ BQ.findFareForCancelledBookings cancelledBookingIds
        BQ.findFareForCancelledBookings cancelledBookingIds
      _ -> pure $ driverStats.earningsMissed + fromMaybe 0 rideFare
  -- Esq.runNoTransaction $ DSQ.setDriverStats (cast driverId) updatedTotalRideCount cancelledCount earningMissed
  DSQ.setDriverStats (cast driverId) updatedTotalRideCount cancelledCount earningMissed
  pure $ driverStats {DS.ridesCancelled = Just cancelledCount, DS.earningsMissed = earningMissed}
  where
    getTotalRideCount = do
      prevTotal <-
        maybe
          ( do
              -- allRides <- Esq.runInReplica $ RQ.findAllRidesByDriverId driverId
              allRides <- RQ.findAllRidesByDriverId driverId
              pure $ length allRides
          )
          (\_ -> pure 1)
          driverStats.totalRidesAssigned
      let curTotal = fromMaybe 0 driverStats.totalRidesAssigned
      pure $ prevTotal + curTotal
