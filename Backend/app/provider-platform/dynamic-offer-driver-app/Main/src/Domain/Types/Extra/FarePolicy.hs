{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.FarePolicy
  ( module Domain.Types.Extra.FarePolicy,
    module Reexport,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.Merchant as DPM
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

data SchedulingCharge
  = ProgressiveSchedulingCharge Double
  | ConstantSchedulingCharge HighPrecMoney
  deriving (Generic, Show, Eq, FromJSON, Read, Ord, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''SchedulingCharge)

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
  = RideFare
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
  | DeadKmFareComponent
  | ExtraKmFareComponent
  | RideDurationFareComponent
  | TimeBasedFareComponent
  | DistBasedFareComponent
  | TimeFareComponent
  | DistanceFareComponent
  | PickupChargeComponent
  | ExtraDistanceFareComponent
  | ExtraTimeFareComponent
  | StateEntryPermitChargesComponent
  | AmbulanceDistBasedFareComponent
  | RideVatComponent
  | TollVatComponent
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareChargeConfig = FareChargeConfig
  { value :: Text,
    appliesOn :: [FareChargeComponent]
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

data CongestionChargeDetails = CongestionChargeDetails
  { dpVersion :: Maybe Text,
    mbSupplyDemandRatioToLoc :: Maybe Double,
    mbSupplyDemandRatioFromLoc :: Maybe Double,
    congestionChargePerMin :: Maybe Double,
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARCity :: Maybe Double,
    shadowSurgeMultiplier :: Maybe Centesimal,
    shadowSurgeVersion :: Maybe Int
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

mkCongestionChargeMultiplier :: DPM.CongestionChargeMultiplierAPIEntity -> CongestionChargeMultiplier
mkCongestionChargeMultiplier (DPM.BaseFareAndExtraDistanceFare charge) = BaseFareAndExtraDistanceFare charge
mkCongestionChargeMultiplier (DPM.ExtraDistanceFare charge) = ExtraDistanceFare charge

congestionChargeMultiplierToCentesimal :: CongestionChargeMultiplier -> Centesimal
congestionChargeMultiplierToCentesimal (BaseFareAndExtraDistanceFare charge) = charge
congestionChargeMultiplierToCentesimal (ExtraDistanceFare charge) = charge
