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
    govtCharges :: Maybe Double,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    businessDiscountPercentage :: Maybe Double,
    priorityCharges :: Maybe HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Maybe Seconds,
    tipOptions :: Maybe [Int],
    additionalCongestionCharge :: HighPrecMoney,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    rideExtraTimeChargeGracePeriod :: Maybe Seconds,
    congestionChargeMultiplier :: Maybe CongestionChargeMultiplier,
    perDistanceUnitInsuranceCharge :: Maybe HighPrecMoney,
    cardCharge :: Maybe CardCharge,
    vatChargeConfig :: Maybe FareChargeConfig,
    commissionChargeConfig :: Maybe FareChargeConfig,
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
    driverCancellationPenaltyAmount :: Maybe HighPrecMoney
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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
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
    govtCharges :: Maybe Double,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    businessDiscountPercentage :: Maybe Double,
    priorityCharges :: Maybe HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Maybe Seconds,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    rideExtraTimeChargeGracePeriod :: Maybe Seconds,
    congestionChargeMultiplier :: Maybe CongestionChargeMultiplier,
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
    driverCancellationPenaltyAmount :: Maybe HighPrecMoney
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
