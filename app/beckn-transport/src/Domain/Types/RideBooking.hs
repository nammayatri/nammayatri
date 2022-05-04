module Domain.Types.RideBooking where

import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Servant.API

data RideBookingStatus
  = CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  | SCHEDULED
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
    transactionId :: Text,
    requestId :: Id DSR.SearchRequest,
    quoteId :: Id DQuote.Quote,
    status :: RideBookingStatus,
    providerId :: Id DOrg.Organization,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Id DRD.RiderDetails,
    fromLocationId :: Id DLoc.SearchReqLocation,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    reallocationsCount :: Int,
    rideBookingDetails :: RideBookingDetails,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data RideBookingDetails = OneWayDetails OneWayRideBookingDetails | RentalDetails

data OneWayRideBookingDetails = OneWayRideBookingDetails
  { toLocationId :: Id DLoc.SearchReqLocation,
    estimatedDistance :: Double
  }

-- FIXME do we need to handle wrong variants, when for example toLocationId is Just, estimatedDistance = Nothing?
mkRideBookingDetails :: Maybe (Id DLoc.SearchReqLocation) -> Maybe Double -> RideBookingDetails
mkRideBookingDetails mbDropLocation mbEstimatedDistance = do
  let mbTuple = (,) <$> mbDropLocation <*> mbEstimatedDistance
  maybe RentalDetails (\(toLocationId, estimatedDistance) -> OneWayDetails $ OneWayRideBookingDetails {..}) mbTuple

getDropLocationId :: RideBookingDetails -> Maybe (Id DLoc.SearchReqLocation)
getDropLocationId rideBookingDetails = case rideBookingDetails of
  RentalDetails -> Nothing
  OneWayDetails oneWayDetails -> Just oneWayDetails.toLocationId

getEstimatedDistance :: RideBookingDetails -> Maybe Double
getEstimatedDistance rideBookingDetails = case rideBookingDetails of
  RentalDetails -> Nothing
  OneWayDetails oneWayDetails -> Just oneWayDetails.estimatedDistance
