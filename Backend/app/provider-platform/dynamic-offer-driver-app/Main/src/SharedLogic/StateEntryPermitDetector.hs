{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.StateEntryPermitDetector
  ( JourneySegment (..),
    SegmentChargeResult (..),
    buildJourneySegments,
    buildJourneySegmentsFromActualStops,
    calculateTotalStateEntryPermitCharges,
    calculateSegmentStateEntryPermitCharge,
    constructSegmentTripCategories,
    determineOverallRideType,
  )
where

import qualified Domain.Types as DTC
import qualified Domain.Types.FarePolicy as DFP
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.Common

-- | Data structure representing a journey segment between two points
data JourneySegment = JourneySegment
  { segmentFrom :: LatLong,
    segmentTo :: LatLong,
    segmentIndex :: Int
  }
  deriving (Show, Eq, Generic)

-- | Result of calculating charge for a segment
data SegmentChargeResult = SegmentChargeResult
  { segment :: JourneySegment,
    isIntercity :: Bool,
    isCrossCity :: Bool,
    charge :: Maybe HighPrecMoney
  }
  deriving (Show, Eq, Generic)

-- | Build journey segments from source, stops, and destination
-- e.g., source + [stop1, stop2] + destination -> [(source, stop1, 0), (stop1, stop2, 1), (stop2, destination, 2)]
buildJourneySegments ::
  LatLong -> -- source
  [LatLong] -> -- stops
  Maybe LatLong -> -- destination
  [JourneySegment]
buildJourneySegments _ _ Nothing = []
buildJourneySegments source stops (Just destination) =
  let allPoints = source : stops ++ [destination]
   in zipWith3 JourneySegment (init allPoints) (tail allPoints) [0 ..]

-- | Build journey segments from actual stop information records
-- Used for EndRide recalculation based on actual stops taken
-- Takes pickup location, actual stops taken (sorted by stopOrder), and trip end point
buildJourneySegmentsFromActualStops ::
  LatLong -> -- pickup/source location
  [LatLong] -> -- actual stop locations (extracted from StopInformation.stopStartLatLng, sorted by stopOrder)
  LatLong -> -- trip end point (actual drop location)
  [JourneySegment]
buildJourneySegmentsFromActualStops source actualStops destination =
  let allPoints = source : actualStops ++ [destination]
   in zipWith3 JourneySegment (init allPoints) (tail allPoints) [0 ..]

-- | Calculate total state entry permit charges by summing segment charges
calculateTotalStateEntryPermitCharges :: [SegmentChargeResult] -> Maybe HighPrecMoney
calculateTotalStateEntryPermitCharges segmentResults =
  let totalCharges = sum $ mapMaybe (.charge) segmentResults
   in if totalCharges > 0
        then Just totalCharges
        else Nothing

-- | Calculate state entry permit charge for a single segment
-- Returns the stateEntryPermitCharges from the fare policy if available
-- Note: The caller should ensure this is only called for Intracity or Cross City rides,
-- not for Intercity rides (which don't use segment-based charges)
calculateSegmentStateEntryPermitCharge ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Bool -> -- isIntercity for this segment
  Bool -> -- isCrossCity for this segment
  Maybe DFP.FullFarePolicy -> -- fare policy for this segment (if available)
  JourneySegment ->
  m SegmentChargeResult
calculateSegmentStateEntryPermitCharge isIntercity isCrossCity mbFarePolicy segment = do
  -- Extract stateEntryPermitCharges from the fare policy
  let charge = mbFarePolicy >>= (.stateEntryPermitCharges)
  logDebug $
    "Segment " <> show segment.segmentIndex
      <> ": isIntercity="
      <> show isIntercity
      <> ", isCrossCity="
      <> show isCrossCity
      <> ", charge="
      <> show charge
  return $
    SegmentChargeResult
      { segment = segment,
        isIntercity = isIntercity,
        isCrossCity = isCrossCity,
        charge = charge
      }

-- | Construct trip categories for a segment based on whether it crosses cities
-- Similar logic to getPossibleTripOption but for individual segments
-- Used in both Search and EndRide flows
constructSegmentTripCategories :: Bool -> Bool -> Maybe Text -> Bool -> [DTC.TripCategory]
constructSegmentTripCategories segmentIsIntercity segmentIsCrossCity mbDestinationTravelCityName isScheduled =
  if segmentIsIntercity
    then do
      if segmentIsCrossCity
        then do
          [DTC.CrossCity DTC.OneWayOnDemandStaticOffer mbDestinationTravelCityName]
            <> (if not isScheduled then [DTC.CrossCity DTC.OneWayRideOtp mbDestinationTravelCityName, DTC.CrossCity DTC.OneWayOnDemandDynamicOffer mbDestinationTravelCityName] else [])
        else do
          [DTC.InterCity DTC.OneWayOnDemandStaticOffer mbDestinationTravelCityName]
            <> (if not isScheduled then [DTC.InterCity DTC.OneWayRideOtp mbDestinationTravelCityName, DTC.InterCity DTC.OneWayOnDemandDynamicOffer mbDestinationTravelCityName] else [])
    else do
      -- Intracity segment - use OneWay trip categories
      [DTC.Rental DTC.OnDemandStaticOffer]
        <> (if not isScheduled then [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandDynamicOffer, DTC.Ambulance DTC.OneWayOnDemandDynamicOffer, DTC.Rental DTC.RideOtp, DTC.Delivery DTC.OneWayOnDemandDynamicOffer] else [DTC.OneWay DTC.OneWayRideOtp, DTC.OneWay DTC.OneWayOnDemandStaticOffer])

-- | Determine overall ride type from trip categories
-- Returns (isInterCity, isCrossCity) based on the trip categories
-- Used in both Search and EndRide flows
determineOverallRideType :: [DTC.TripCategory] -> (Bool, Bool)
determineOverallRideType tripCategories =
  let hasInterCity = any (\cat -> case cat of DTC.InterCity _ _ -> True; _ -> False) tripCategories
      hasCrossCity = any (\cat -> case cat of DTC.CrossCity _ _ -> True; _ -> False) tripCategories
   in (hasInterCity && not hasCrossCity, hasCrossCity)
