{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.EstimateCalculation where

import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.EstimateRevised as DER
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.TransporterConfig as DTMT
import qualified Domain.Types.QuoteRevised as DQR
import qualified Domain.Types.SearchRequest as DSR
import Environment
import EulerHS.Prelude hiding (id, map)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Tools.Maps as Maps

getDistanceAndDuration :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> LatLong -> Maybe Meters -> Maybe Seconds -> Flow (Meters, Seconds)
getDistanceAndDuration _ _ _ _ (Just distance) (Just duration) = return (distance, duration)
getDistanceAndDuration merchantId merchantOpCityId fromLocation toLocation _ _ = do
  response <-
    Maps.getDistance merchantId merchantOpCityId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  return (response.distance, response.duration)

buildEstimateRevised ::
  (EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) =>
  Id DSR.SearchRequest ->
  UTCTime ->
  Bool ->
  Maybe Meters ->
  Maybe Text ->
  HighPrecMoney ->
  Bool ->
  DFP.FullFarePolicy ->
  m DER.EstimateRevised
buildEstimateRevised searchReqId startTime isScheduled mbDistance specialLocationTag customerCancellationDues nightShiftOverlapChecking fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = fullFarePolicy,
          actualDistance = Just dist,
          rideTime = startTime,
          waitingTime = Nothing,
          actualRideDuration = Nothing,
          avgSpeedOfVehicle = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          nightShiftCharge = Nothing,
          customerCancellationDues = customerCancellationDues,
          nightShiftOverlapChecking = nightShiftOverlapChecking,
          estimatedDistance = Nothing,
          estimatedRideDuration = Nothing,
          timeDiffFromUtc = Nothing
        }
  let baseFare = fareSum fareParams
  logDebug $ "baseFare: " <> show baseFare
  estimateId <- Id <$> generateGUID
  now <- getCurrentTime
  void $ cacheFarePolicyByEstimateId estimateId.getId fullFarePolicy
  let mbDriverExtraFeeBounds = DFP.findDriverExtraFeeBoundsByDistance dist <$> fullFarePolicy.driverExtraFeeBounds
  pure
    DER.EstimateRevised
      { id = estimateId,
        requestId = searchReqId,
        vehicleVariant = fullFarePolicy.vehicleVariant,
        tripCategory = fullFarePolicy.tripCategory,
        estimatedDistance = mbDistance,
        minFare = baseFare + maybe 0 (.minFee) mbDriverExtraFeeBounds,
        maxFare = baseFare + maybe 0 (.maxFee) mbDriverExtraFeeBounds,
        fareParams = Just fareParams,
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        specialLocationTag = specialLocationTag,
        isScheduled = isScheduled,
        createdAt = now,
        updatedAt = now
      }

getPossibleTripOption :: UTCTime -> DTMT.TransporterConfig -> DSearch.DSearchReq -> DTC.TripOption
getPossibleTripOption now tConf dsReq = do
  let (schedule, isScheduled) =
        if tConf.scheduleRideBufferTime `addUTCTime` now < dsReq.pickupTime
          then (dsReq.pickupTime, True)
          else (now, False)
      tripCategories =
        case dsReq.dropLocation of
          Just _ ->
            [DTC.OneWay DTC.OneWayOnDemandStaticOffer, DTC.RoundTrip DTC.OnDemandStaticOffer, DTC.Rental DTC.OnDemandStaticOffer]
              <> (if not isScheduled then [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer, DTC.RoundTrip DTC.RideOtp, DTC.Rental DTC.RideOtp] else [])
          Nothing ->
            [DTC.Rental DTC.OnDemandStaticOffer]
              <> [DTC.Rental DTC.RideOtp | not isScheduled]

  DTC.TripOption {..}

buildQuoteRevised ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSR.SearchRequest ->
  Id DM.Merchant ->
  UTCTime ->
  Bool ->
  Maybe Meters ->
  Maybe Seconds ->
  Maybe Text ->
  HighPrecMoney ->
  Bool ->
  DFP.FullFarePolicy ->
  m DQR.QuoteRevised
buildQuoteRevised searchRequest transporterId pickupTime isScheduled mbDistance mbDuration specialLocationTag customerCancellationDues nightShiftOverlapChecking fullFarePolicy = do
  let dist = fromMaybe 0 mbDistance
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = fullFarePolicy,
          actualDistance = Just dist,
          rideTime = pickupTime,
          waitingTime = Nothing,
          actualRideDuration = Nothing,
          avgSpeedOfVehicle = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing,
          nightShiftCharge = Nothing,
          customerCancellationDues = customerCancellationDues,
          nightShiftOverlapChecking = nightShiftOverlapChecking,
          estimatedDistance = searchRequest.estimatedDistance,
          estimatedRideDuration = searchRequest.estimatedDuration,
          timeDiffFromUtc = Nothing
        }
  quoteId <- Id <$> generateGUID
  void $ cacheFarePolicyByQuoteId quoteId.getId fullFarePolicy
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = (\duration -> fromIntegral duration `addUTCTime` now) <$> mbDuration
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DQR.QuoteRevised
      { id = quoteId,
        searchRequestId = searchRequest.id,
        providerId = transporterId,
        distance = mbDistance,
        vehicleVariant = fullFarePolicy.vehicleVariant,
        tripCategory = fullFarePolicy.tripCategory,
        farePolicy = Just $ DFP.fullFarePolicyToFarePolicy fullFarePolicy,
        createdAt = now,
        updatedAt = now,
        ..
      }
