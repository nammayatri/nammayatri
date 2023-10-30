{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Booking.Type where

import Data.Aeson
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalDetails as DRentalDetails
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

data BookingStatusObj = BookingStatusObj
  { normalBooking :: [BookingStatus],
    rentalBooking :: [BookingStatus]
  }

activeBookingStatusObj :: BookingStatusObj
activeBookingStatusObj =
  BookingStatusObj
    { normalBooking = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED],
      rentalBooking = [TRIP_ASSIGNED]
    }

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | REALLOCATED
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)

data BPPBooking

data Booking = Booking
  { id :: Id Booking,
    transactionId :: Text,
    driverId :: Maybe Text,
    fulfillmentId :: Maybe Text,
    bppBookingId :: Maybe (Id BPPBooking),
    quoteId :: Maybe (Id DQuote.Quote),
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    paymentUrl :: Maybe Text,
    status :: BookingStatus,
    providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    providerName :: Text,
    providerMobileNumber :: Text,
    primaryExophone :: Text,
    startTime :: UTCTime,
    riderId :: Id DPerson.Person,
    fromLocation :: DLoc.Location,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    vehicleVariant :: VehicleVariant,
    bookingDetails :: BookingDetails,
    tripTerms :: Maybe DTripTerms.TripTerms,
    merchantId :: Id DMerchant.Merchant,
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data BookingDetails
  = OneWayDetails OneWayBookingDetails
  | RentalDetails BaseDuration DRentalDetails.RentalDetails
  | DriverOfferDetails OneWayBookingDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneBookingDetails
  deriving (Show)

newtype BaseDuration = BaseDuration Hours
  deriving (Show)

data OneWayBookingDetails = OneWayBookingDetails
  { toLocation :: DLoc.Location,
    distance :: HighPrecMeters
  }
  deriving (Show)

data OneWaySpecialZoneBookingDetails = OneWaySpecialZoneBookingDetails
  { toLocation :: DLoc.Location,
    distance :: HighPrecMeters,
    otpCode :: Maybe Text
  }
  deriving (Show)
