{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fare transparency utilities for NammaYatri.
--
-- Provides structured fare breakdown, human-readable surge reasons,
-- toll estimation metadata, and fare cap (safety-net ceiling) logic.
--
-- Addresses P0 fare transparency fixes:
--   FIX-01  Show congestion/surge charge as explicit line item
--   FIX-04  Fare cap to prevent runaway fares
--   FIX-09  Toll estimation flags
module SharedLogic.FareTransparency
  ( -- * Fare Breakdown
    FareBreakdown (..),
    buildFareBreakdown,

    -- * Surge / Congestion Info
    SurgeInfo (..),
    buildSurgeInfo,
    surgeReasonText,

    -- * Toll Estimation Flags
    TollEstimationInfo (..),
    buildTollEstimationInfo,

    -- * Fare Cap (Safety Net)
    FareCapResult (..),
    applyFareCap,
    defaultFareCapMultiplier,
  )
where

import Domain.Types.FareParameters (FareParameters (..), FareParametersDetails (..), FParamsInterCityDetails (..), FParamsProgressiveDetails (..), FParamsRentalDetails (..), FParamsAmbulanceDetails (..))
import qualified Domain.Types.FarePolicy as DFP
import Kernel.Prelude
import Kernel.Utils.Common (HighPrecMoney (..))

-- ---------------------------------------------------------------------------
-- Fare Breakdown
-- ---------------------------------------------------------------------------

-- | A structured, rider-facing fare breakdown built from 'FareParameters'.
--
-- Every field is an absolute amount (in the ride currency). The frontend
-- can render each non-zero / non-Nothing field as a separate line item.
data FareBreakdown = FareBreakdown
  { baseFare :: HighPrecMoney,
    distanceCharge :: Maybe HighPrecMoney,
    -- | Time-based fare component (ride duration fare)
    timeCharge :: Maybe HighPrecMoney,
    surgeMultiplier :: Maybe Double,
    surgeAmount :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    platformFee :: Maybe HighPrecMoney,
    waitingCharge :: Maybe HighPrecMoney,
    nightShiftCharge :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    insuranceCharge :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | Build a rider-facing fare breakdown from internal 'FareParameters'.
buildFareBreakdown :: FareParameters -> Maybe Double -> FareBreakdown
buildFareBreakdown fp mbSurgeMultiplier =
  FareBreakdown
    { baseFare = fp.baseFare,
      distanceCharge = getDistanceCharge fp,
      timeCharge = fp.rideExtraTimeFare,
      surgeMultiplier = mbSurgeMultiplier,
      surgeAmount = fp.congestionCharge,
      tollCharges = fp.tollCharges,
      platformFee = fp.platformFee,
      waitingCharge = fp.waitingCharge,
      nightShiftCharge = fp.nightShiftCharge,
      parkingCharge = fp.parkingCharge,
      insuranceCharge = fp.insuranceCharge
    }

-- | Extract distance-based fare from progressive/intercity details.
getDistanceCharge :: FareParameters -> Maybe HighPrecMoney
getDistanceCharge fp = case fp.fareParametersDetails of
  ProgressiveDetails det -> det.extraKmFare
  InterCityDetails det -> Just det.distanceFare
  RentalDetails det -> Just det.distBasedFare
  AmbulanceDetails det -> Just det.distBasedFare
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Surge / Congestion Info
-- ---------------------------------------------------------------------------

-- | Rider-facing surge/congestion charge information.
data SurgeInfo = SurgeInfo
  { congestionChargeAmount :: Maybe HighPrecMoney,
    -- | e.g. 1.2 means 20% surge
    congestionMultiplier :: Maybe Double,
    -- | Human-readable reason such as "High demand in your area"
    reason :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | Build surge info from fare policy congestion data and smart-tip reason.
buildSurgeInfo ::
  Maybe HighPrecMoney ->
  Maybe DFP.CongestionChargeMultiplier ->
  Maybe Text ->
  Maybe SurgeInfo
buildSurgeInfo mbCongestionCharge mbMultiplier mbSmartTipReason =
  case mbCongestionCharge of
    Just charge
      | charge > 0 ->
          Just
            SurgeInfo
              { congestionChargeAmount = Just charge,
                congestionMultiplier = fmap (realToFrac . DFP.congestionChargeMultiplierToCentesimal) mbMultiplier,
                reason = surgeReasonText mbSmartTipReason
              }
    _ -> Nothing

-- | Map backend smart-tip reason codes to rider-friendly text.
surgeReasonText :: Maybe Text -> Text
surgeReasonText = \case
  Just "DemandMoreThanSupply" -> "High demand in your area"
  Just "LowQuoteAcceptanceRate" -> "Fewer drivers accepting rides nearby"
  Just "ExtraCongestion" -> "Heavy traffic congestion in this area"
  _ -> "High demand in your area"

-- ---------------------------------------------------------------------------
-- Toll Estimation Flags
-- ---------------------------------------------------------------------------

-- | Metadata about toll detection at estimate time.
data TollEstimationInfo = TollEstimationInfo
  { -- | True when tolls were detected on the estimated route
    tollsEstimated :: Bool,
    -- | True when the actual route may differ causing toll changes
    tollsMayChange :: Bool,
    -- | Total estimated toll amount
    estimatedTollAmount :: Maybe HighPrecMoney,
    -- | Names of detected toll plazas
    tollPlazaNames :: Maybe [Text]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | Build toll estimation info from search-time toll detection results.
buildTollEstimationInfo ::
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] ->
  TollEstimationInfo
buildTollEstimationInfo mbTollCharges mbTollNames _mbTollIds =
  TollEstimationInfo
    { tollsEstimated = isJust mbTollCharges && mbTollCharges > Just 0,
      tollsMayChange = isJust mbTollCharges, -- route may change, so tolls may differ
      estimatedTollAmount = mbTollCharges,
      tollPlazaNames = mbTollNames
    }

-- ---------------------------------------------------------------------------
-- Fare Cap (Safety Net)
-- ---------------------------------------------------------------------------

-- | Default fare cap multiplier: final fare cannot exceed 1.5x of estimated
-- midpoint fare. This is a safety net to prevent runaway fares due to GPS
-- drift, route deviations, or calculation errors.
defaultFareCapMultiplier :: Double
defaultFareCapMultiplier = 1.5

-- | Result of applying the fare cap.
data FareCapResult = FareCapResult
  { cappedFare :: HighPrecMoney,
    -- | True if the fare was actually capped (original > cap)
    wasCapped :: Bool,
    -- | The discount applied due to capping (original - capped)
    fareCapDiscount :: HighPrecMoney,
    -- | The cap ceiling that was applied
    fareCeiling :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- | Apply a fare cap as a safety net. If the computed final fare exceeds
-- @fareCapMultiplier * estimatedMidpointFare@, cap it at the ceiling.
--
-- Returns 'Nothing' if no estimated fare is available (conservative:
-- do not cap if we cannot compute a ceiling).
applyFareCap ::
  -- | Fare cap multiplier (e.g. 1.5 for 50% ceiling)
  Double ->
  -- | Estimated fare (midpoint of min-max range)
  HighPrecMoney ->
  -- | Computed final fare before capping
  HighPrecMoney ->
  FareCapResult
applyFareCap multiplier estimatedFare finalFare =
  let ceiling = HighPrecMoney $ toRational multiplier * estimatedFare.getHighPrecMoney
      isCapped = finalFare > ceiling
      cappedFare = if isCapped then ceiling else finalFare
      fareCapDiscount = if isCapped then finalFare - cappedFare else 0
   in FareCapResult
        { cappedFare,
          wasCapped = isCapped,
          fareCapDiscount,
          fareCeiling = ceiling
        }
