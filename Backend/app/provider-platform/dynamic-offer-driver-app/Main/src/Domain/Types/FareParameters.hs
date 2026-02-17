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

import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.FarePolicy as FP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
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
    stopCharges :: Maybe HighPrecMoney,
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
    petCharges :: Maybe HighPrecMoney,
    driverAllowance :: Maybe HighPrecMoney,
    businessDiscount :: Maybe HighPrecMoney,
    personalDiscount :: Maybe HighPrecMoney,
    priorityCharges :: Maybe HighPrecMoney,
    congestionChargeViaDp :: Maybe HighPrecMoney,
    insuranceCharge :: Maybe HighPrecMoney,
    cardCharge :: Maybe CardCharge,
    luggageCharge :: Maybe HighPrecMoney,
    returnFeeCharge :: Maybe HighPrecMoney,
    boothCharge :: Maybe HighPrecMoney,
    platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    platformFeeChargesBy :: FP.PlatformFeeMethods,
    currency :: Currency,
    updatedAt :: UTCTime,
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    conditionalCharges :: [DAC.ConditionalCharges],
    shouldApplyBusinessDiscount :: Bool,
    shouldApplyPersonalDiscount :: Bool,
    driverCancellationPenaltyAmount :: Maybe HighPrecMoney,
    -- | Payment processing fee (blended or method-specific)
    -- TODO: Will be enhanced when payment context is available
    paymentProcessingFee :: Maybe HighPrecMoney,
    -- | VAT charge calculated based on vat_charge_config in fare_policy
    -- Populated by calculateFareParametersV2. Included in pureFareSum.
    rideVat :: Maybe HighPrecMoney,
    -- | VAT on toll charges calculated based on toll_tax_charge_config in fare_policy
    -- Populated by calculateFareParametersV2. Included in pureFareSum.
    tollVat :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow, FromJSON, ToJSON, ToSchema)

data CardCharge = CardCharge
  { onFare :: Maybe HighPrecMoney,
    fixed :: Maybe HighPrecMoney
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

type FullFareParametersProgressiveDetails = (Id FareParameters, FParamsProgressiveDetails)

type FullFareParametersRentalDetails = (Id FareParameters, FParamsRentalDetails)

type FullFareParametersInterCityDetails = (Id FareParameters, FParamsInterCityDetails)

type FullFareParametersAmbulanceDetails = (Id FareParameters, FParamsAmbulanceDetails)

data FareParametersType = Progressive | Slab | Rental | InterCity | Ambulance
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FareParametersType)

getFareParametersType :: FareParameters -> FareParametersType
getFareParametersType fareParams = case fareParams.fareParametersDetails of
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
  AmbulanceDetails _ -> Ambulance
