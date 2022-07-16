module Domain.Types.RideBooking where

import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Domain.Types.DriverQuote
import Domain.Types.FareParams (FareParameters)
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.RideBooking.BookingLocation as DLoc
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import Servant.API

data RideBookingStatus
  = NEW
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data RideBooking = RideBooking
  { id :: Id RideBooking,
    quoteId :: Id DriverQuote,
    status :: RideBookingStatus,
    providerId :: Id DOrg.Organization,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLoc.BookingLocation,
    toLocation :: DLoc.BookingLocation,
    vehicleVariant :: DVeh.Variant,
    estimatedDistance :: HighPrecMeters,
    fareParams :: FareParameters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)
