{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FarePolicy
  ( module Reexport,
    module Domain.Types.FarePolicy,
    ReturnFee (..),
    BoothCharge (..),
    FareChargeComponent (..),
    FareChargeConfig (..),
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as DPM
import Data.Aeson.Types
import qualified Data.List as DL
import Data.List.NonEmpty
import Data.Text as Text
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.CancellationFarePolicy as DTC
import qualified Domain.Types.ConditionalCharges as DTAC
import Domain.Types.FarePolicy.DriverExtraFeeBounds as Reexport
import Domain.Types.FarePolicy.FarePolicyAmbulanceDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyInterCityDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyRentalDetails as Reexport
import Domain.Types.FarePolicy.FarePolicySlabsDetails as Reexport
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.GenericPretty
import qualified Lib.Types.SpecialLocation as SL
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum, mkBeamInstancesForJSON)

data ReturnFee
  = ReturnFeeFixed HighPrecMoney
  | ReturnFeePercentage Double
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''ReturnFee)

data BoothCharge
  = BoothChargeFixed HighPrecMoney
  | BoothChargePercentage Double
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''BoothCharge)

data FarePolicyD (s :: DTC.UsageSafety) = FarePolicy
  { id :: Id FarePolicy,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    perStopCharge :: Maybe HighPrecMoney,
    perLuggageCharge :: Maybe HighPrecMoney,
    returnFee :: Maybe ReturnFee,
    boothCharges :: Maybe BoothCharge,
    currency :: Currency,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    distanceUnit :: DistanceUnit,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    driverAllowance :: Maybe HighPrecMoney,
    airportConvenienceFee :: Maybe HighPrecMoney,
    businessDiscountPercentage :: Maybe Double,
    personalDiscountPercentage :: Maybe Double,
    priorityCharges :: Maybe HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Maybe Seconds,
    tipOptions :: Maybe [Int],
    additionalCongestionCharge :: HighPrecMoney,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    rideExtraTimeChargeGracePeriod :: Maybe Seconds,
    congestionChargeMultiplier :: Maybe CongestionChargeMultiplier,
    fareRecomputeCapEnabled :: Maybe Bool,
    fareRecomputeCapConfig :: Maybe FareRecomputeCapConfig,
    perDistanceUnitInsuranceCharge :: Maybe HighPrecMoney,
    cardCharge :: Maybe CardCharge,
    vatChargeConfig :: Maybe FareChargeConfig,
    commissionChargeConfig :: Maybe FareChargeConfig,
    cancellationCommissionChargeConfig :: Maybe FareChargeConfig,
    tollTaxChargeConfig :: Maybe FareChargeConfig,
    farePolicyDetails :: FarePolicyDetailsD s,
    cancellationFarePolicyId :: Maybe (Id DTC.CancellationFarePolicy),
    description :: Maybe Text,
    platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    platformFeeChargesBy :: PlatformFeeMethods,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    conditionalCharges :: [DTAC.ConditionalCharges],
    driverCancellationNotAllowed :: Maybe Bool
  }
  deriving (Generic, Show, ToSchema)

data AllowedTripDistanceBounds = AllowedTripDistanceBounds
  { maxAllowedTripDistance :: Meters,
    minAllowedTripDistance :: Meters,
    distanceUnit :: DistanceUnit
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

mkAllowedTripDistanceBounds :: DistanceUnit -> DPM.AllowedTripDistanceBoundsAPIEntity -> AllowedTripDistanceBounds
mkAllowedTripDistanceBounds distanceUnit DPM.AllowedTripDistanceBoundsAPIEntity {..} =
  AllowedTripDistanceBounds
    { maxAllowedTripDistance = maybe maxAllowedTripDistance distanceToMeters maxAllowedTripDistanceWithUnit,
      minAllowedTripDistance = maybe minAllowedTripDistance distanceToMeters minAllowedTripDistanceWithUnit,
      distanceUnit
    }

type FarePolicy = FarePolicyD 'DTC.Safe

instance FromJSON (FarePolicyD 'DTC.Unsafe)

instance ToJSON (FarePolicyD 'DTC.Unsafe)

-- FIXME remove
instance FromJSON (FarePolicyD 'DTC.Safe)

-- FIXME remove
instance ToJSON (FarePolicyD 'DTC.Safe)

data FarePolicyDetailsD (s :: DTC.UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s) | RentalDetails (FPRentalDetailsD s) | InterCityDetails (FPInterCityDetailsD s) | AmbulanceDetails (FPAmbulanceDetailsD s)
  deriving (Generic, Show, ToSchema)

type FarePolicyDetails = FarePolicyDetailsD 'DTC.Safe

instance FromJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance ToJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance FromJSON (FarePolicyDetailsD 'DTC.Safe)

instance ToJSON (FarePolicyDetailsD 'DTC.Safe)

data CardCharge = CardCharge
  { perDistanceUnitMultiplier :: Maybe Double,
    fixed :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareChargeComponent
  = RideFare -- base fare without any charges
  | WaitingCharge
  | ServiceChargeComponent
  | TollChargesComponent
  | CongestionChargeComponent
  | ParkingChargeComponent
  | PetChargeComponent
  | PriorityChargeComponent
  | NightShiftChargeComponent
  | InsuranceChargeComponent
  | StopChargeComponent
  | LuggageChargeComponent
  | PlatformFeeComponent
  | CustomerCancellationChargeComponent
  | CustomerExtraFeeComponent
  | -- Progressive details
    DeadKmFareComponent
  | ExtraKmFareComponent
  | RideDurationFareComponent
  | -- Rental details
    TimeBasedFareComponent
  | DistBasedFareComponent
  | -- InterCity details
    TimeFareComponent
  | DistanceFareComponent
  | PickupChargeComponent
  | ExtraDistanceFareComponent
  | ExtraTimeFareComponent
  | StateEntryPermitChargesComponent
  | -- Ambulance details
    AmbulanceDistBasedFareComponent
  | -- VAT components
    RideVatComponent
  | TollVatComponent
  | -- Components not covered by any of the above (added for per-component fare-recompute capping)
    DriverAllowanceComponent
  | AirportConvenienceFeeComponent
  | ReturnFeeChargeComponent
  | BoothChargeComponent
  | RideExtraTimeFareComponent
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Configuration for a charge (VAT, commission, or toll tax)
--
-- Example JSON:
--   {
--     "value": "14%",  -- Percentage charge (e.g., "14%") or fixed amount (e.g., "50")
--     "appliesOn": ["RideFare", "DeadKmFareComponent"]  -- Components to apply charge on
--   }
--
-- The charge will be calculated as:
-- - If percentage: (sum of appliesOn component values) * (percentage / 100)
-- - If fixed: the fixed amount itself
data FareChargeConfig = FareChargeConfig
  { value :: Text, -- Charge value: percentage like "14%" or fixed like "50"
    appliesOn :: [FareChargeComponent] -- List of fare components to apply charge on
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | The ceiling a component may reach, from its estimate alone.
--
-- 'PercentCap' and 'FixedCap' both express the *allowance* — how much a
-- component may grow beyond its estimate — not the final capped value:
--   capped estimate = estimate + capAllowance strategy estimate
--
-- 'Frozen' components cannot grow past their estimate at all (allowance 0).
-- 'Derived' components (GST) are not capped independently — GST already
-- accounts for the buffer on its taxable base, so no separate buffer is
-- required; its final value is recomputed from the capped base instead.
data CapStrategy
  = PercentCap PercentCapCfg
  | FixedCap FixedCapCfg
  | Frozen
  | Derived
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Allowance = estimate * percent / 100, clamped to [minCapAmount, maxCapAmount]
-- when those bounds are configured. Example: distance fare estimate = 2500,
-- percent = 10 (raw allowance 250), maxCapAmount = 100 -> capped estimate = 2600,
-- not 2750 -- the bound clamps the allowance, not the final fare.
data PercentCapCfg = PercentCapCfg
  { percent :: Double,
    minCapAmount :: Maybe HighPrecMoney,
    maxCapAmount :: Maybe HighPrecMoney
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Allowance is this flat amount regardless of the component's estimate size
-- (e.g. waiting charge, toll: often estimated at 0, so a percentage would give
-- no headroom -- a fixed rupee buffer is used instead). No min/max: a constant
-- has nothing to clamp.
newtype FixedCapCfg = FixedCapCfg
  { amount :: HighPrecMoney
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | One cap rule, applied to a group of components -- mirrors 'FareChargeConfig's
-- {value, appliesOn} shape rather than a per-component map, so admins can grant
-- the same strategy to several components (e.g. TimeBasedFareComponent and
-- DistBasedFareComponent both under the same PercentCap) in one entry.
data FareRecomputeCap = FareRecomputeCap
  { strategy :: CapStrategy,
    appliesOn :: [FareChargeComponent]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Per-fare-policy (per service tier) recompute-cap configuration. A component
-- with no matching rule here is left unconfigured -- pass-through, unbounded --
-- per the agreed "not all components need a configured buffer" behaviour.
newtype FareRecomputeCapConfig = FareRecomputeCapConfig
  { caps :: [FareRecomputeCap]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | The configured strategy for a component, if any rule in the config applies to it.
lookupCapStrategy :: FareRecomputeCapConfig -> FareChargeComponent -> Maybe CapStrategy
lookupCapStrategy capConfig component =
  strategy <$> KP.find (\cap -> component `KP.elem` cap.appliesOn) capConfig.caps

-- | Validate a parsed cap config before it's persisted (dashboard/CSV upsert).
-- Nothing downstream catches these — 'lookupCapStrategy' resolves an overlapping
-- component by silently taking the first matching rule in list order, and
-- 'capAllowance' silently lets 'maxCapAmount' win over a misconfigured
-- 'minCapAmount' -- so a bad config authored via CSV would otherwise apply
-- with no feedback to the person who wrote it.
validateFareRecomputeCapConfig :: FareRecomputeCapConfig -> Either Text ()
validateFareRecomputeCapConfig capConfig = do
  KP.mapM_ validateCap capConfig.caps
  validateNoOverlap (KP.concatMap (.appliesOn) capConfig.caps)
  where
    validateCap cap = case cap.strategy of
      PercentCap cfg -> do
        KP.when (cfg.percent < 0) $ Left $ "Fare recompute cap: percent must be >= 0, got " <> KP.show cfg.percent
        KP.when (cfg.percent > 100) $ Left $ "Fare recompute cap: percent must be <= 100, got " <> KP.show cfg.percent
        validateNonNegative "minCapAmount" cfg.minCapAmount
        validateNonNegative "maxCapAmount" cfg.maxCapAmount
        case (cfg.minCapAmount, cfg.maxCapAmount) of
          (Just minAmt, Just maxAmt) ->
            KP.when (minAmt > maxAmt) $
              Left $ "Fare recompute cap: minCapAmount (" <> KP.show minAmt <> ") must not exceed maxCapAmount (" <> KP.show maxAmt <> ")"
          _ -> Right ()
      FixedCap cfg -> validateNonNegative "amount" (Just cfg.amount)
      Frozen -> Right ()
      Derived -> Right ()

    validateNonNegative label = KP.maybe (Right ()) $ \amt ->
      KP.when (amt < 0) $ Left $ "Fare recompute cap: " <> label <> " must be >= 0, got " <> KP.show amt

    -- Components appearing under more than one cap rule are ambiguous: 'lookupCapStrategy'
    -- would silently pick whichever rule comes first in the list, ignoring the rest.
    validateNoOverlap allComponents =
      let duplicates = DL.nub (KP.filter (\c -> KP.length (KP.filter (== c) allComponents) > 1) allComponents)
       in KP.unless (KP.null duplicates) $
            Left $ "Fare recompute cap: component(s) appear in more than one cap rule (ambiguous -- first match wins): " <> KP.show duplicates

-- | How much a component's estimate may grow under its configured strategy.
-- See 'CapStrategy' for what each constructor means.
capAllowance :: CapStrategy -> HighPrecMoney -> HighPrecMoney
capAllowance capStrategy estimate = case capStrategy of
  Frozen -> 0
  Derived -> 0
  FixedCap cfg -> cfg.amount
  PercentCap cfg ->
    let rawAllowance = estimate * realToFrac cfg.percent / 100
        flooredAllowance = maybe rawAllowance (`max` rawAllowance) cfg.minCapAmount
     in maybe flooredAllowance (`min` flooredAllowance) cfg.maxCapAmount

-- | Ceiling = estimate + allowance. Used both to size allocation-time buffers
-- (the estimate raised to its ceiling) and to cap the recomputed value at
-- end-ride (recomputed value clamped to this ceiling) -- the same configured
-- strategy drives both, per the business requirement that one configured
-- value serves as "buffer for ride allocation AND max cap of fare recompute".
capByStrategy :: Maybe CapStrategy -> HighPrecMoney -> HighPrecMoney -> HighPrecMoney
capByStrategy Nothing _ recomputedValue = recomputedValue -- unconfigured: pass through, unbounded
capByStrategy (Just capStrategy) estimate recomputedValue =
  min recomputedValue (estimate + capAllowance capStrategy estimate)

data CongestionChargeMultiplier
  = BaseFareAndExtraDistanceFare Centesimal
  | ExtraDistanceFare Centesimal
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PlatformFeeMethods = Subscription | FixedAmount | None | SlabBased | NoCharge
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlatformFeeMethods

data FarePolicyType = Progressive | Slabs | Rental | InterCity | Ambulance
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FarePolicyType)
$(mkBeamInstancesForJSON ''CongestionChargeMultiplier)
$(mkBeamInstancesForJSON ''FareRecomputeCapConfig)
$(mkBeamInstancesForEnum ''PlatformFeeMethods)

data FullFarePolicyD (s :: DTC.UsageSafety) = FullFarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id Merchant,
    vehicleServiceTier :: DVST.ServiceTierType,
    tripCategory :: DTC.TripCategory,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe HighPrecMoney,
    perStopCharge :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    perLuggageCharge :: Maybe HighPrecMoney,
    returnFee :: Maybe ReturnFee,
    boothCharges :: Maybe BoothCharge,
    currency :: Currency,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    tipOptions :: Maybe [Int],
    distanceUnit :: DistanceUnit,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    driverAllowance :: Maybe HighPrecMoney,
    airportConvenienceFee :: Maybe HighPrecMoney,
    businessDiscountPercentage :: Maybe Double,
    personalDiscountPercentage :: Maybe Double,
    priorityCharges :: Maybe HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Maybe Seconds,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    rideExtraTimeChargeGracePeriod :: Maybe Seconds,
    congestionChargeMultiplier :: Maybe CongestionChargeMultiplier,
    fareRecomputeCapEnabled :: Maybe Bool,
    fareRecomputeCapConfig :: Maybe FareRecomputeCapConfig,
    congestionChargePerMin :: Maybe Double,
    dpVersion :: Maybe Text,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    additionalCongestionCharge :: HighPrecMoney,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    perDistanceUnitInsuranceCharge :: Maybe HighPrecMoney,
    cardCharge :: Maybe CardCharge,
    vatChargeConfig :: Maybe FareChargeConfig,
    commissionChargeConfig :: Maybe FareChargeConfig,
    cancellationCommissionChargeConfig :: Maybe FareChargeConfig,
    tollTaxChargeConfig :: Maybe FareChargeConfig,
    farePolicyDetails :: FarePolicyDetailsD s,
    description :: Maybe Text,
    cancellationFarePolicy :: Maybe DTC.CancellationFarePolicy,
    platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    platformFeeChargesBy :: PlatformFeeMethods,
    disableRecompute :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARCity :: Maybe Double,
    conditionalCharges :: [DTAC.ConditionalCharges],
    congestionChargeData :: Maybe CongestionChargeData,
    driverCancellationNotAllowed :: Maybe Bool,
    mbArea :: Maybe SL.Area,
    fareSettlementType :: Maybe SL.FareSettlementType
  }
  deriving (Generic, Show)

type FullFarePolicy = FullFarePolicyD 'DTC.Safe

data CongestionChargeDetails = CongestionChargeDetails
  { dpVersion :: Maybe Text,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    congestionChargePerMin :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARCity :: Maybe Double
  }
  deriving (Generic, Show)

data CongestionChargeData = CongestionChargeData
  { mbActualQARFromLocGeohashDistancePast :: Maybe Double,
    mbActualQARFromLocGeohashPast :: Maybe Double,
    mbActualQARCityPast :: Maybe Double,
    mbCongestionFromLocGeohashDistance :: Maybe Double,
    mbCongestionFromLocGeohashDistancePast :: Maybe Double,
    mbCongestionFromLocGeohash :: Maybe Double,
    mbCongestionFromLocGeohashPast :: Maybe Double,
    mbCongestionCity :: Maybe Double,
    mbCongestionCityPast :: Maybe Double,
    mbActualQARFromLocGeohashDistance :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromJSON (FullFarePolicyD 'DTC.Unsafe)

instance ToJSON (FullFarePolicyD 'DTC.Unsafe)

instance FromJSON FullFarePolicy

instance ToJSON FullFarePolicy

type FullDriverExtraFeeBounds = (Id FarePolicy, DriverExtraFeeBounds)

type FullFarePolicyProgressiveDetails = (Id FarePolicy, FPProgressiveDetails)

type FullFarePolicyRentalDetails = (Id FarePolicy, FPRentalDetails)

type FullFarePolicyInterCityDetails = (Id FarePolicy, FPInterCityDetails)

mkCongestionChargeMultiplier :: DPM.CongestionChargeMultiplierAPIEntity -> CongestionChargeMultiplier
mkCongestionChargeMultiplier (DPM.BaseFareAndExtraDistanceFare charge) = BaseFareAndExtraDistanceFare charge
mkCongestionChargeMultiplier (DPM.ExtraDistanceFare charge) = ExtraDistanceFare charge

farePolicyToFullFarePolicy :: Id Merchant -> DVST.ServiceTierType -> DTC.TripCategory -> Maybe DTC.CancellationFarePolicy -> CongestionChargeDetails -> Maybe CongestionChargeData -> FarePolicy -> Maybe Bool -> FullFarePolicy
farePolicyToFullFarePolicy merchantId' vehicleServiceTier tripCategory cancellationFarePolicy CongestionChargeDetails {..} congestionChargeData FarePolicy {..} disableRecompute =
  FullFarePolicy
    { merchantId = merchantId',
      mbArea = Nothing,
      fareSettlementType = Nothing,
      ..
    }

fullFarePolicyToFarePolicy :: FullFarePolicy -> FarePolicy
fullFarePolicyToFarePolicy ffp@FullFarePolicy {..} =
  let cancellationFarePolicyId = (.id) <$> ffp.cancellationFarePolicy
   in FarePolicy
        { merchantId = Just merchantId,
          ..
        }

getFarePolicyType :: FarePolicy -> FarePolicyType
getFarePolicyType farePolicy = case farePolicy.farePolicyDetails of
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
  AmbulanceDetails _ -> Ambulance

congestionChargeMultiplierToCentesimal :: CongestionChargeMultiplier -> Centesimal
congestionChargeMultiplierToCentesimal (BaseFareAndExtraDistanceFare charge) = charge
congestionChargeMultiplierToCentesimal (ExtraDistanceFare charge) = charge
