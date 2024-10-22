{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Ride
  ( module Dashboard.ProviderPlatform.Management.Ride,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.Ride
import Dashboard.Common as Reexport
import Dashboard.Common.Booking as Reexport (CancellationReasonCode (..))
import Dashboard.Common.Ride as Reexport
import Data.Aeson
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Predicate (UniqueField (UniqueField))
import Kernel.Utils.JSON (constructorsWithLowerCase)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant hiding (Summary)

---------------------------------------------------------
-- ride list --------------------------------------------

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

---------------------------------------------------------
-- ride start -------------------------------------------

type RideStartAPI =
  Capture "rideId" (Id Ride)
    :> "start"
    :> ReqBody '[JSON] StartRideReq
    :> Post '[JSON] APISuccess

data StartRideReq = StartRideReq
  { point :: Maybe LatLong,
    odometerReadingValue :: Maybe Centesimal
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets StartRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- ride end ---------------------------------------------

type RideEndAPI =
  Capture "rideId" (Id Ride)
    :> "end"
    :> ReqBody '[JSON] EndRideReq
    :> Post '[JSON] APISuccess

data EndRideReq = EndRideReq
  { point :: Maybe LatLong,
    odometerReadingValue :: Maybe Centesimal
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets EndRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- multiple ride end ------------------------------

validateMultipleRideEndReq :: Validate MultipleRideEndReq
validateMultipleRideEndReq MultipleRideEndReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

-- active ride id on the basis of vehicle number ---------------------------------------------

type CurrentActiveRideAPI =
  Capture "vehicleNumber" Text
    :> "currentActiveRide"
    :> Get '[JSON] (Id Ride)

-- ride cancel ------------------------------------------

type RideCancelAPI =
  Capture "rideId" (Id Ride)
    :> "cancel"
    :> ReqBody '[JSON] CancelRideReq
    :> Post '[JSON] APISuccess

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CancelRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- multiple ride cancel ---------------------------

validateMultipleRideCancelReq :: Validate MultipleRideCancelReq
validateMultipleRideCancelReq MultipleRideCancelReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

---------------------------------------------------------
-- Booking with driver phone number and vehicle number ---------------------------------------

type BookingWithVehicleNumberAndPhoneAPI =
  "booking"
    :> "withVehicleNumberAndPhone"
    :> ReqBody '[JSON] BookingWithVehicleAndPhoneReq
    :> Post '[JSON] BookingWithVehicleAndPhoneRes

data BookingWithVehicleAndPhoneReq = BookingWithVehicleAndPhoneReq
  { vehicleNumber :: Text,
    phoneNumber :: Text,
    countryCode :: Text,
    endRideForDriver :: Bool,
    endRideForVehicle :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets BookingWithVehicleAndPhoneReq where
  hideSecrets = identity

newtype BookingWithVehicleAndPhoneRes = BookingWithVehicleAndPhoneRes
  { driverId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets BookingWithVehicleAndPhoneRes where
  hideSecrets = identity

-- ticket ride list --------------------------------------------

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

deriving anyclass instance FromJSON TicketRideListRes

deriving anyclass instance ToJSON TicketRideListRes

instance FromJSON RideInfo where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase

--- fare breakup -------------------------------

type FareBreakUpAPI =
  Capture "rideId" (Id Ride)
    :> "fareBreakUp"
    :> Get '[JSON] FareBreakUpRes

data FareBreakUpRes = FareBreakUpRes
  { estimatedFareBreakUp :: Maybe FareBreakUp,
    actualFareBreakUp :: Maybe FareBreakUp
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets FareBreakUpRes where
  hideSecrets = identity

data FareBreakUp = FareBreakUp
  { driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money,
    serviceCharge :: Maybe Money,
    govtCharges :: Maybe Money,
    baseFare :: Money,
    waitingCharge :: Maybe Money,
    rideExtraTimeFare :: Maybe Money,
    nightShiftCharge :: Maybe Money,
    driverSelectedFareWithCurrency :: Maybe PriceAPIEntity,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    serviceChargeWithCurrency :: Maybe PriceAPIEntity,
    govtChargesWithCurrency :: Maybe PriceAPIEntity,
    baseFareWithCurrency :: PriceAPIEntity,
    waitingChargeWithCurrency :: Maybe PriceAPIEntity,
    rideExtraTimeFareWithCurrency :: Maybe PriceAPIEntity,
    nightShiftChargeWithCurrency :: Maybe PriceAPIEntity,
    nightShiftRateIfApplies :: Maybe Double,
    fareParametersDetails :: FareParametersDetails,
    customerCancellationDues :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    congestionCharge :: Maybe Money,
    customerCancellationDuesWithCurrency :: Maybe PriceAPIEntity,
    tollChargesWithCurrency :: Maybe PriceAPIEntity,
    congestionChargeWithCurrency :: Maybe PriceAPIEntity,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FareParametersDetails = ProgressiveDetails FParamsProgressiveDetails | SlabDetails FParamsSlabDetails | RentalDetails FParamsRentalDetails | InterCityDetails FParamsInterCityDetails | AmbulanceDetails FParamsAmbulanceDetails
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsProgressiveDetails = FParamsProgressiveDetails
  { deadKmFare :: Money,
    extraKmFare :: Maybe Money,
    deadKmFareWithCurrency :: PriceAPIEntity,
    extraKmFareWithCurrency :: Maybe PriceAPIEntity,
    rideDurationFareWithCurrency :: Maybe PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsAmbulanceDetails = FParamsAmbulanceDetails
  { platformFee :: Maybe PriceAPIEntity,
    sgst :: Maybe PriceAPIEntity,
    cgst :: Maybe PriceAPIEntity,
    distBasedFare :: PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsSlabDetails = FParamsSlabDetails
  { platformFee :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    platformFeeWithCurrency :: Maybe PriceAPIEntity,
    sgstWithCurrency :: Maybe PriceAPIEntity,
    cgstWithCurrency :: Maybe PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsRentalDetails = FParamsRentalDetails
  { timeBasedFare :: Money,
    distBasedFare :: Money,
    deadKmFare :: PriceAPIEntity,
    timeBasedFareWithCurrency :: PriceAPIEntity,
    distBasedFareWithCurrency :: PriceAPIEntity,
    extraDistance :: Meters,
    extraDistanceWithUnit :: Distance,
    extraDuration :: Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FParamsInterCityDetails = FParamsInterCityDetails
  { timeFare :: PriceAPIEntity,
    distanceFare :: PriceAPIEntity,
    pickupCharge :: PriceAPIEntity,
    extraDistanceFare :: PriceAPIEntity,
    extraTimeFare :: PriceAPIEntity
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
