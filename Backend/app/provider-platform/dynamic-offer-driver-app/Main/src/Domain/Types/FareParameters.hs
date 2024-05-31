{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FareParameters where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow (..))
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data FareParameters = FareParameters
  { id :: Id FareParameters,
    driverSelectedFare :: Maybe HighPrecMoney,
    customerExtraFee :: Maybe HighPrecMoney,
    serviceCharge :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    govtCharges :: Maybe HighPrecMoney,
    baseFare :: HighPrecMoney,
    waitingCharge :: Maybe HighPrecMoney,
    rideExtraTimeFare :: Maybe HighPrecMoney,
    nightShiftCharge :: Maybe HighPrecMoney,
    nightShiftRateIfApplies :: Maybe Double,
    fareParametersDetails :: FareParametersDetails,
    customerCancellationDues :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    congestionCharge :: Maybe HighPrecMoney,
    currency :: Currency,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FareParametersDetails = ProgressiveDetails FParamsProgressiveDetails | SlabDetails FParamsSlabDetails | RentalDetails FParamsRentalDetails | InterCityDetails FParamsInterCityDetails
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data FParamsProgressiveDetails = FParamsProgressiveDetails
  { deadKmFare :: HighPrecMoney,
    extraKmFare :: Maybe HighPrecMoney,
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
    extraTimeFare :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

type FullFareParametersProgressiveDetails = (Id FareParameters, FParamsProgressiveDetails)

type FullFareParametersRentalDetails = (Id FareParameters, FParamsRentalDetails)

type FullFareParametersInterCityDetails = (Id FareParameters, FParamsInterCityDetails)

data FareParametersType = Progressive | Slab | Rental | InterCity
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FareParametersType)

getFareParametersType :: FareParameters -> FareParametersType
getFareParametersType fareParams = case fareParams.fareParametersDetails of
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
