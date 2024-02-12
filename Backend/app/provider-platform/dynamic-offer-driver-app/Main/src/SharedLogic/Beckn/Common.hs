{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Beckn.Common where

import Domain.Types.Booking as DRB
-- import Kernel.Types.Id

import qualified Domain.Types.BookingCancellationReason as SBCR
-- import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import Domain.Types.Vehicle as SVeh
import Kernel.External.Maps.Types as Maps
import Kernel.Prelude

data BookingDetails = BookingDetails
  { ride :: DRide.Ride,
    booking :: DRB.Booking,
    driver :: SP.Person,
    vehicle :: SVeh.Vehicle
  }

data DRideAssignedReq = DRideAssignedReq
  { bookingDetails :: BookingDetails,
    image :: Maybe Text,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool
  }

-- newtype DStopArrivedBuildReq = DStopArrivedBuildReq -----------
--   { bookingDetails :: BookingDetails
--   }

data DRideStartedReq = DRideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe Maps.LatLong
  }

data DRideCompletedReq = DRideCompletedReq
  { bookingDetails :: BookingDetails,
    fareParams :: Fare.FareParameters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe Maps.LatLong
  }

data DBookingCancelledReq = DBookingCancelledReq
  { booking :: DRB.Booking,
    bookingDetails :: Maybe BookingDetails,
    cancellationSource :: SBCR.CancellationSource
  }

data DDriverArrivedReq = DDriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime
  }

-- data DEstimateRepetitionReq = DEstimateRepetitionReq -----------
--   { bookingDetails :: BookingDetails,
--     estimateId :: Id DEst.Estimate,
--     cancellationSource :: SBCR.CancellationSource
--   }

-- data DNewMessageReq = DNewMessageReq -----------
--   { bookingDetails :: BookingDetails,
--     message :: Text
--   }

-- data DSafetyAlertReq = DSafetyAlertReq -----------
--   { bookingDetails :: BookingDetails,
--     code :: Text,
--     reason :: Text
--   }
