{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.FareParameters where

import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow (..))
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data CardCharge = CardCharge
  { onFare :: Maybe HighPrecMoney,
    fixed :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data CustomerGateFeeItem = CustomerGateFeeItem
  { itemName :: Text,
    amount :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FareParametersDetails = ProgressiveDetails FParamsProgressiveDetails | SlabDetails FParamsSlabDetails | RentalDetails FParamsRentalDetails | InterCityDetails FParamsInterCityDetails | AmbulanceDetails FParamsAmbulanceDetails
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FParamsProgressiveDetails = FParamsProgressiveDetails
  { deadKmFare :: HighPrecMoney,
    extraKmFare :: Maybe HighPrecMoney,
    rideDurationFare :: Maybe HighPrecMoney,
    currency :: Currency
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FParamsSlabDetails = FParamsSlabDetails
  { platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    currency :: Currency
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FParamsAmbulanceDetails = FParamsAmbulanceDetails
  { platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    distBasedFare :: HighPrecMoney,
    currency :: Currency
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FParamsRentalDetails = FParamsRentalDetails
  { timeBasedFare :: HighPrecMoney,
    distBasedFare :: HighPrecMoney,
    currency :: Currency,
    extraDistance :: Meters,
    distanceUnit :: DistanceUnit,
    extraDuration :: Seconds,
    deadKmFare :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FParamsInterCityDetails = FParamsInterCityDetails
  { timeFare :: HighPrecMoney,
    distanceFare :: HighPrecMoney,
    pickupCharge :: HighPrecMoney,
    currency :: Currency,
    extraDistanceFare :: HighPrecMoney,
    stateEntryPermitCharges :: Maybe HighPrecMoney,
    extraTimeFare :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FareParametersType = Progressive | Slab | Rental | InterCity | Ambulance
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FareParametersType)

getFareParametersType :: FareParametersDetails -> FareParametersType
getFareParametersType = \case
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
  AmbulanceDetails _ -> Ambulance
