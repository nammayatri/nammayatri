{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FarePolicy (module Reexport, module Domain.Types.FarePolicy) where

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

data FarePolicyD (s :: DTC.UsageSafety) = FarePolicy
  { id :: Id FarePolicy,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    perStopCharge :: Maybe HighPrecMoney,
    currency :: Currency,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    distanceUnit :: DistanceUnit,
    govtCharges :: Maybe Double,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    tipOptions :: Maybe [Int],
    additionalCongestionCharge :: HighPrecMoney,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    congestionChargeMultiplier :: Maybe CongestionChargeMultiplier,
    perDistanceUnitInsuranceCharge :: Maybe HighPrecMoney,
    cardCharge :: Maybe CardCharge,
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
    conditionalCharges :: [DTAC.ConditionalCharges]
  }
  deriving (Generic, Show)

data AllowedTripDistanceBounds = AllowedTripDistanceBounds
  { maxAllowedTripDistance :: Meters,
    minAllowedTripDistance :: Meters,
    distanceUnit :: DistanceUnit
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

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
  deriving (Generic, Show)

type FarePolicyDetails = FarePolicyDetailsD 'DTC.Safe

instance FromJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance ToJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance FromJSON (FarePolicyDetailsD 'DTC.Safe)

instance ToJSON (FarePolicyDetailsD 'DTC.Safe)

data CardCharge = CardCharge
  { perDistanceUnitMultiplier :: Maybe Double,
    fixed :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CongestionChargeMultiplier
  = BaseFareAndExtraDistanceFare Centesimal
  | ExtraDistanceFare Centesimal
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
    currency :: Currency,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    tipOptions :: Maybe [Int],
    distanceUnit :: DistanceUnit,
    govtCharges :: Maybe Double,
    tollCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
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
    congestionChargeData :: Maybe CongestionChargeData
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
