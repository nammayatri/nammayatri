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
    SegmentCalculationContext (..),
    buildJourneySegments,
    buildJourneySegmentsFromActualStops,
    calculateTotalStateEntryPermitCharges,
    calculateSegmentStateEntryPermitCharge,
    constructSegmentTripCategory,
    determineOverallRideType,
    calculateStateEntryPermitChargesWithSegments,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types as DTC
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Maps.Types (LatLong)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.CityDetector as CityDetector
import qualified SharedLogic.FarePolicy as SFP
import Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import Utils.Common.Cac.KeyNameConstants (CacKey (..))

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

-- | Context data needed for calculating segment-based state entry permit charges
-- This encapsulates all the parameters needed to call getFarePolicy for each segment
data SegmentCalculationContext = SegmentCalculationContext
  { merchant :: DM.Merchant,
    transporterConfig :: DTConf.TransporterConfig,
    isDashboardRequest :: Bool,
    transactionId :: Text,
    vehicleServiceTier :: DTC.ServiceTierType,
    dynamicPricingLogicVersion :: Maybe Int,
    configInExperimentVersions :: [LYT.ConfigVersionMap],
    isScheduled :: Bool -- Whether the overall journey is scheduled (used for trip category determination)
  }
  deriving (Generic)

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

-- | Construct trip category for a segment based on whether it crosses cities
-- Returns the appropriate trip category for a single segment
-- Uses the same logic as getPossibleTripOption to ensure consistency
constructSegmentTripCategory :: Bool -> Bool -> Maybe Text -> Bool -> DTC.TripCategory
constructSegmentTripCategory segmentIsIntercity segmentIsCrossCity mbDestinationTravelCityName isScheduled =
  if segmentIsIntercity
    then
      if segmentIsCrossCity
        then DTC.CrossCity DTC.OneWayOnDemandStaticOffer mbDestinationTravelCityName
        else DTC.InterCity DTC.OneWayOnDemandStaticOffer mbDestinationTravelCityName
    else -- Intracity segment - use Rental for scheduled, OneWay for on-demand
    -- This matches the original getPossibleTripOption logic

      if isScheduled
        then DTC.Rental DTC.OnDemandStaticOffer
        else DTC.OneWay DTC.OneWayRideOtp

-- | Determine overall ride type from trip categories
-- Returns (isInterCity, isCrossCity) based on the trip categories
-- Used in both Search and EndRide flows
determineOverallRideType :: [DTC.TripCategory] -> (Bool, Bool)
determineOverallRideType tripCategories =
  let hasInterCity = any (\cat -> case cat of DTC.InterCity _ _ -> True; _ -> False) tripCategories
      hasCrossCity = any (\cat -> case cat of DTC.CrossCity _ _ -> True; _ -> False) tripCategories
   in (hasInterCity && not hasCrossCity, hasCrossCity)

-- | Main function to calculate state entry permit charges for journeys with multiple segments
-- This function encapsulates all the logic for segment-based calculation
-- Returns Nothing if:
--   - Single segment (no stops) - caller should use farePolicy.stateEntryPermitCharges
--   - Intercity non-cross-city ride - no segment charges apply
--   - No charges calculated from segments
calculateStateEntryPermitChargesWithSegments ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Log m,
    ServiceFlow m r,
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  SegmentCalculationContext ->
  LatLong -> -- source
  [LatLong] -> -- stops
  Maybe LatLong -> -- destination
  [DTC.TripCategory] -> -- trip categories for overall ride type
  Text -> -- flow name for logging (e.g., "Search", "EndRide", "EditDestination")
  m (Maybe HighPrecMoney)
calculateStateEntryPermitChargesWithSegments ctx source stops mbDestination tripCategories flowName = do
  let journeySegments = buildJourneySegments source stops mbDestination
  let (isOverallIntercity, isOverallCrossCity) = determineOverallRideType tripCategories

  if length journeySegments <= 1
    then do
      -- Simple A-to-B (no stops): return Nothing to use fare policy's stateEntryPermitCharges
      logDebug $ "[STATE_ENTRY_PERMIT][" <> flowName <> "] Single segment (no stops), using fare policy's stateEntryPermitCharges"
      return Nothing
    else
      if isOverallIntercity && not isOverallCrossCity
        then do
          logDebug $ "[STATE_ENTRY_PERMIT][" <> flowName <> "] Overall intercity (non-cross-city), skipping segment-based stateEntryPermitCharges"
          return Nothing
        else do
          segmentChargeResults <- processSegmentsForCharges ctx journeySegments flowName
          let totalCharges = calculateTotalStateEntryPermitCharges segmentChargeResults
          logInfo $ "[STATE_ENTRY_PERMIT][" <> flowName <> "] Calculated segment charges: segments=" <> show (length journeySegments) <> ", totalCharges=" <> show totalCharges
          return totalCharges

-- | Process all segments to calculate charges for each
-- Internal helper function called by calculateStateEntryPermitChargesWithSegments
processSegmentsForCharges ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Log m,
    ServiceFlow m r,
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  SegmentCalculationContext ->
  [JourneySegment] ->
  Text -> -- flow name for logging
  m [SegmentChargeResult]
processSegmentsForCharges ctx journeySegments flowName = do
  forM journeySegments $ \segment -> do
    logDebug $ "[STATE_ENTRY_PERMIT][" <> flowName <> "] evaluating segment from=" <> show segment.segmentFrom <> " to=" <> show segment.segmentTo

    -- Get source city information for this segment
    segmentSourceCityResult <- CityDetector.getNearestOperatingAndSourceCity ctx.merchant segment.segmentFrom
    segmentMerchantOpCity <- CQMOC.getMerchantOpCity ctx.merchant (Just segmentSourceCityResult.nearestOperatingCity.city)

    -- Get transporter config for THIS segment's source merchant operating city
    -- This is important because intercity/crosscity detection depends on the source city's config
    segmentTransporterConfig <- CCT.findByMerchantOpCityId segmentMerchantOpCity.id (Just (TransactionId (Id ctx.transactionId))) >>= fromMaybeM (TransporterConfigDoesNotExist segmentMerchantOpCity.id.getId)

    -- Check if this segment crosses city/state boundaries using segment-specific config
    (segmentIsIntercity, segmentIsCrossCity, mbSegmentDestinationTravelCityName) <-
      CityDetector.checkForIntercityOrCrossCity segmentTransporterConfig (Just segment.segmentTo) segmentSourceCityResult.sourceCity ctx.merchant

    logDebug $ "[STATE_ENTRY_PERMIT][" <> flowName <> "] segment classification isIntercity=" <> show segmentIsIntercity <> ", isCrossCity=" <> show segmentIsCrossCity <> ", destTravelCityName=" <> show mbSegmentDestinationTravelCityName

    -- Get fare policy for this segment if it crosses boundaries
    mbSegmentFarePolicy <-
      if segmentIsIntercity || segmentIsCrossCity
        then do
          let segmentTripCategory = constructSegmentTripCategory segmentIsIntercity segmentIsCrossCity mbSegmentDestinationTravelCityName ctx.isScheduled

          -- Use getFarePolicy directly to get the fare policy for this segment
          segmentFarePolicy <-
            SFP.getFarePolicy
              (Just segment.segmentFrom)
              (Just segment.segmentTo)
              Nothing -- fromLocGeohash (optional)
              Nothing -- toLocGeohash (optional)
              Nothing -- distance (optional)
              Nothing -- duration (optional)
              segmentMerchantOpCity.id
              ctx.isDashboardRequest
              segmentTripCategory
              ctx.vehicleServiceTier
              Nothing -- area (optional)
              Nothing -- booking start time (optional)
              ctx.dynamicPricingLogicVersion
              (Just (TransactionId (Id ctx.transactionId)))
              ctx.configInExperimentVersions

          return $ Just segmentFarePolicy
        else return Nothing

    calculateSegmentStateEntryPermitCharge segmentIsIntercity segmentIsCrossCity mbSegmentFarePolicy segment
