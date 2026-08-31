{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | EndRide coins / incentive-journey side-effects.
-- Invoked from kafka-consumers RIDE_EVENTS_CONSUMER (best-effort RideEndedEvent),
-- not from the EndRide HTTP path, so EndRide latency is unaffected.
-- Same branching as the former EndRide fork: fraud block, metro/valid-ride counters,
-- user_cohort_mapping vs Incentive# / driverCoinsEvent, DynamicOffer vs OTP.
module SharedLogic.RideEvents.DriverCoinsAndJourney
  ( processRideEndedCoinsAndJourney,
  )
where

import qualified Beckn.OnDemand.Utils.Common as BODUC
import qualified Data.Text as Text
import Data.Time (utctDay)
import qualified Domain.Types as DTC
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RD
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Domain.Types.VehicleVariant as DTVeh
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.IncentiveJourney as SLJourney
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.RiderDriverCorrelation as QRiderDriverCorrelation
import Tools.Utils (isValidRide)

processRideEndedCoinsAndJourney ::
  ( MonadFlow m,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    ClickhouseFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    Finance.HasActorInfo m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    PaymentBeamFlow.BeamFlow m r,
    MonadReader r m
  ) =>
  DRide.Ride ->
  SRB.Booking ->
  DTConf.TransporterConfig ->
  m ()
processRideEndedCoinsAndJourney ride booking thresholdConfig = do
  let driverId = ride.driverId
  mbDriver <- QP.findById driverId
  now <- getCurrentTime
  shouldFlagRiderForRepeatCustomerFraud <- computeShouldFlagRiderForRepeatCustomerFraud driverId booking thresholdConfig now ride
  when shouldFlagRiderForRepeatCustomerFraud $ QRiderDetails.flagRiderForCoinZero booking.riderId
  expirationPeriod <- DC.getExpirationSeconds thresholdConfig.timeDiffFromUtc
  let validRideTaken = isValidRide ride
      metroRideType = determineMetroRideType booking.specialLocationTag "SureMetro" "SureWarriorMetro"
  logDebug $ "RideEnded coins/journey MetroRideType : " <> show metroRideType
  dailyCoinsAlreadyBlocked <- isDriverCoinsBlockedForDay driverId
  let shouldBlockCoins = shouldFlagRiderForRepeatCustomerFraud || dailyCoinsAlreadyBlocked
  if shouldBlockCoins
    then blockDriverCoinsForToday driverId thresholdConfig.timeDiffFromUtc
    else do
      when (DCT.isMetroRideType metroRideType && validRideTaken) $ do
        DC.incrementMetroRideCount driverId metroRideType expirationPeriod 1
      when (DTC.isDynamicOfferTrip booking.tripCategory && validRideTaken) $ do
        DC.incrementValidRideCount driverId expirationPeriod 1
        let earningsDelta = maybe 0 (roundToIntegral . getHighPrecMoney) ride.fare
            distanceDelta = maybe 0 getMeters ride.chargeableDistance
            rideTimeDelta =
              fromMaybe 0 $
                (\start end -> max 0 (roundToIntegral (diffUTCTime end start)))
                  <$> ride.tripStartTime
                  <*> ride.tripEndTime
            vehCategory = DTVeh.getVehicleCategoryFromVehicleVariantDefault ride.vehicleVariant
            timeBoundReferenceUtc = fromMaybe ride.createdAt ride.tripStartTime
            mbPickupSpecialLocationId = booking.area >>= SL.pickupSpecialZoneIdFromArea
            mbDropSpecialLocationId = booking.area >>= SL.dropSpecialZoneIdFromArea
            rideDeltas =
              IncentiveMetrics.RideIncentiveDeltas
                { ridesDelta = 1,
                  earningsDelta,
                  distanceMetersDelta = distanceDelta,
                  rideTimeSecondsDelta = rideTimeDelta
                }
        DC.incrementValidRideCountForTimeBoundCohort
          driverId
          booking.providerId
          booking.merchantOperatingCityId
          vehCategory
          DCT.DynamicOfferTrip
          expirationPeriod
          thresholdConfig.timeDiffFromUtc
          timeBoundReferenceUtc
        DC.incrementScopedValidRideCounts DCT.DynamicOfferTrip driverId booking.vehicleServiceTier mbPickupSpecialLocationId mbDropSpecialLocationId expirationPeriod
        -- Assigned journeys take precedence: skip Incentive# / legacy coin flow.
        hasJourneys <- SLJourney.hasAssignedJourneys driverId
        if hasJourneys
          then
            SLJourney.evaluateDriverJourney
              driverId
              booking.providerId
              booking.merchantOperatingCityId
              thresholdConfig
              vehCategory
              (Just booking.vehicleServiceTier)
              (Just ride.id.getId)
              mbPickupSpecialLocationId
              mbDropSpecialLocationId
              timeBoundReferenceUtc
              rideDeltas
          else DC.driverCoinsEvent driverId mbDriver booking.providerId booking.merchantOperatingCityId (DCT.EndRide (isJust booking.disabilityTag) (booking.coinsRewardedOnGoldTierRide) ride metroRideType DCT.DynamicOfferTrip) (Just ride.id.getId) ride.vehicleVariant (Just booking.vehicleServiceTier) (Just booking.configInExperimentVersions) booking.area
      when (DTC.isRideOtpTrip booking.tripCategory && validRideTaken) $ do
        DC.incrementOTPValidRideCount driverId expirationPeriod 1
        let vehCategory = DTVeh.getVehicleCategoryFromVehicleVariantDefault ride.vehicleVariant
            timeBoundReferenceUtc = fromMaybe ride.createdAt ride.tripStartTime
            mbPickupSpecialLocationId = booking.area >>= SL.pickupSpecialZoneIdFromArea
            mbDropSpecialLocationId = booking.area >>= SL.dropSpecialZoneIdFromArea
        DC.incrementScopedValidRideCounts DCT.OTPRideTrip driverId booking.vehicleServiceTier mbPickupSpecialLocationId mbDropSpecialLocationId expirationPeriod
        DC.incrementValidRideCountForTimeBoundCohort
          driverId
          booking.providerId
          booking.merchantOperatingCityId
          vehCategory
          DCT.OTPRideTrip
          expirationPeriod
          thresholdConfig.timeDiffFromUtc
          timeBoundReferenceUtc
        hasJourneys <- SLJourney.hasAssignedJourneys driverId
        unless hasJourneys $
          DC.driverCoinsEvent driverId mbDriver booking.providerId booking.merchantOperatingCityId (DCT.EndRide (isJust booking.disabilityTag) (booking.coinsRewardedOnGoldTierRide) ride metroRideType DCT.OTPRideTrip) (Just ride.id.getId) ride.vehicleVariant (Just booking.vehicleServiceTier) (Just booking.configInExperimentVersions) booking.area

computeShouldFlagRiderForRepeatCustomerFraud ::
  (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Id DP.Person ->
  SRB.Booking ->
  DTConf.TransporterConfig ->
  UTCTime ->
  DRide.Ride ->
  m Bool
computeShouldFlagRiderForRepeatCustomerFraud driverId booking thresholdConfig now ride = do
  riderBlockedForCoins <- QRiderDetails.isRiderFlaggedForCoinZero booking.riderId
  isFavouritePair <- isFavouriteDriverRiderPair (cast driverId) booking.riderId
  if isFavouritePair
    then pure False
    else do
      let merchantLocalDay = utctDay $ addUTCTime (secondsToNominalDiffTime thresholdConfig.timeDiffFromUtc) now
      priorRidesSameCustomer <- QRide.countPriorCompletedRidesWithSameCustomer (cast driverId) booking.riderId ride.id merchantLocalDay thresholdConfig.sameRiderDriverRideCountLookbackDays
      let exceededLookback = riderBlockedForCoins || priorRidesSameCustomer > thresholdConfig.sameRiderDriverRideCountThreshold
      pure exceededLookback

isFavouriteDriverRiderPair ::
  (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Maybe (Id RD.RiderDetails) ->
  m Bool
isFavouriteDriverRiderPair driverId' mbRiderDetailsId = case mbRiderDetailsId of
  Nothing -> pure False
  Just riderDetailsId -> isJust <$> QRiderDriverCorrelation.checkRiderFavDriver riderDetailsId driverId' True

mkDriverCoinsBlockedForDayKey :: Id DP.Person -> Text
mkDriverCoinsBlockedForDayKey id = "driverCoins:blocked:today:dId:" <> id.getId

isDriverCoinsBlockedForDay :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> m Bool
isDriverCoinsBlockedForDay id = do
  mbBlocked <- Redis.withCrossAppRedis $ Redis.safeGet (mkDriverCoinsBlockedForDayKey id)
  pure $ fromMaybe False (mbBlocked :: Maybe Bool)

blockDriverCoinsForToday ::
  (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, ClickhouseFlow m r, MonadReader r m) =>
  Id DP.Person ->
  Seconds ->
  m ()
blockDriverCoinsForToday id timeDiffFromUtc = do
  expirationPeriod <- DC.getExpirationSeconds timeDiffFromUtc
  DC.resetTodayCoinsAndAdjustLifetime id timeDiffFromUtc
  Redis.withCrossAppRedis $ do
    Redis.setExp (mkDriverCoinsBlockedForDayKey id) True expirationPeriod

determineMetroRideType :: Maybe Text -> Text -> Text -> DCT.MetroRideType
determineMetroRideType mbSplLocTag sureMetro sureWarriorMetro =
  case mbSplLocTag of
    Just splLocTag ->
      case (fromMetro, toMetro, priorityTag) of
        (True, _, _) -> DCT.FromOrToMetro
        (_, True, _) -> DCT.FromOrToMetro
        _ -> DCT.None
      where
        tagArr = Text.splitOn "_" splLocTag
        sourceTag = tagArr BODUC.!? 0
        destTag = tagArr BODUC.!? 1
        priorityTag = tagArr BODUC.!? 2
        fromMetro = sourceTag == Just sureMetro || sourceTag == Just sureWarriorMetro
        toMetro = destTag == Just sureMetro || destTag == Just sureWarriorMetro
    Nothing -> DCT.None
