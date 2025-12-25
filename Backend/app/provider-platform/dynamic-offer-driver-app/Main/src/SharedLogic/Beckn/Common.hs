{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Beckn.Common where

import Domain.Types.BecknConfig as DBC
import Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import Domain.Types.SearchTry as DST
import Domain.Types.Vehicle as SVeh
import Kernel.External.Maps.Types as Maps
import Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common as Common

data BookingDetails = BookingDetails
  { ride :: DRide.Ride,
    booking :: DRB.Booking,
    driver :: SP.Person,
    driverStats :: DStats.DriverStats,
    vehicle :: SVeh.Vehicle,
    riderPhone :: Maybe Text,
    isValueAddNP :: Bool,
    bppConfig :: DBC.BecknConfig,
    merchant :: DM.Merchant,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    paymentUrl :: Maybe Text
  }

data DRideAssignedReq = DRideAssignedReq
  { bookingDetails :: BookingDetails,
    image :: Maybe Text,
    isDriverBirthDay :: Bool,
    vehicleAge :: Maybe Months,
    isFreeRide :: Bool,
    driverAccountId :: Maybe Payment.AccountId,
    estimateId :: Maybe Text,
    isAlreadyFav :: Bool,
    favCount :: Int,
    isSafetyPlus :: Bool
  }

data DRideStartedReq = DRideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe Maps.LatLong,
    estimateId :: Maybe Text
  }

data DRideCompletedReq = DRideCompletedReq
  { bookingDetails :: BookingDetails,
    fareParams :: Fare.FareParameters,
    tripEndLocation :: Maybe Maps.LatLong,
    estimateId :: Maybe Text
  }

data DBookingCancelledReq = DBookingCancelledReq
  { booking :: DRB.Booking,
    bookingDetails :: Maybe BookingDetails,
    cancellationSource :: SBCR.CancellationSource,
    cancellationFee :: Maybe Common.PriceAPIEntity,
    estimateId :: Maybe Text
  }

data DDriverArrivedReq = DDriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime,
    estimateId :: Maybe Text
  }

data CurrentSearchInfo = CurrentSearchInfo
  { routeDistance :: Maybe Meters,
    dropLocation :: Maybe LatLong,
    searchTry :: DST.SearchTry
  }
  deriving (Generic, Show)
