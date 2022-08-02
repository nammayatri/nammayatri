{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Booking where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Servant.API

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPBooking

data Booking = Booking
  { id :: Id Booking,
    bppBookingId :: Maybe (Id BPPBooking),
    status :: BookingStatus,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    startTime :: UTCTime,
    riderId :: Id DPerson.Person,
    fromLocation :: DLoc.BookingLocation,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    vehicleVariant :: VehicleVariant,
    bookingDetails :: BookingDetails,
    tripTerms :: Maybe DTripTerms.TripTerms,
    merchantId :: Id DMerchant.Merchant,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data BookingDetails
  = OneWayDetails OneWayBookingDetails
  | RentalDetails DRentalSlab.RentalSlab
  | DriverOfferDetails OneWayBookingDetails
  deriving (Show)

data OneWayBookingDetails = OneWayBookingDetails
  { toLocation :: DLoc.BookingLocation,
    distance :: HighPrecMeters
  }
  deriving (Show)
