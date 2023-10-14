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
    driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money,
    serviceCharge :: Maybe Money,
    govtCharges :: Maybe Money,
    baseFare :: Money,
    waitingCharge :: Maybe Money,
    rideExtraTimeFare :: Maybe Money,
    nightShiftCharge :: Maybe Money,
    nightShiftRateIfApplies :: Maybe Double,
    fareParametersDetails :: FareParametersDetails
  }
  deriving (Generic, Show, Eq, PrettyShow)

data FareParametersDetails = ProgressiveDetails FParamsProgressiveDetails | SlabDetails FParamsSlabDetails
  deriving (Generic, Show, Eq, PrettyShow)

data FParamsProgressiveDetails = FParamsProgressiveDetails
  { deadKmFare :: Money,
    extraKmFare :: Maybe Money
  }
  deriving (Generic, Show, Eq, PrettyShow)

data FParamsSlabDetails = FParamsSlabDetails
  { platformFee :: Maybe Money,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow)

type FullFareParametersProgressiveDetails = (Id FareParameters, FParamsProgressiveDetails)

data FareParametersType = Progressive | Slab
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FareParametersType)

getFareParametersType :: FareParameters -> FareParametersType
getFareParametersType fareParams = case fareParams.fareParametersDetails of
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
