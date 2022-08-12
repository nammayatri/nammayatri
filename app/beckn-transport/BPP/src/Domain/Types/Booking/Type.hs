module Domain.Types.Booking.Type where

import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
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
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Booking = Booking
  { id :: Id Booking,
    status :: BookingStatus,
    providerId :: Id DOrg.Organization,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLoc.BookingLocation,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    reallocationsCount :: Int,
    bookingDetails :: BookingDetails,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data BookingDetails = OneWayDetails OneWayBookingDetails | RentalDetails RentalBookingDetails
  deriving (Generic, Eq)

data OneWayBookingDetails = OneWayBookingDetails
  { toLocation :: DLoc.BookingLocation,
    estimatedDistance :: Meters
  }
  deriving (Eq)

newtype RentalBookingDetails = RentalBookingDetails
  { rentalFarePolicyId :: Id DRentalFP.RentalFarePolicy
  }
  deriving (Eq)

mkRentalBookingDetails :: Id DRentalFP.RentalFarePolicy -> BookingDetails
mkRentalBookingDetails rentalFarePolicyId = RentalDetails $ RentalBookingDetails {..}

getFareProductType :: BookingDetails -> SFP.FareProductType
getFareProductType = \case
  OneWayDetails _ -> SFP.ONE_WAY
  RentalDetails _ -> SFP.RENTAL
