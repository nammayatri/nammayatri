{-# LANGUAGE TypeApplications #-}

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
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import GHC.Records.Extra
import Servant.API

data RideBookingStatus
  = CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data OneWayRideBooking = OneWayRideBooking
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
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    toLocationId :: Id DLoc.SearchReqLocation,
    estimatedDistance :: Double
  }
  deriving (Generic)

data RentalRideBooking = RentalRideBooking
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
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data RideBooking = OneWay OneWayRideBooking | Rental RentalRideBooking

getDropLocationId :: RideBooking -> Maybe (Id DLoc.SearchReqLocation)
getDropLocationId (OneWay rideBooking) = Just rideBooking.toLocationId
getDropLocationId (Rental _) = Nothing

getEstimatedDistance :: RideBooking -> Maybe Double
getEstimatedDistance (OneWay rideBooking) = Just rideBooking.estimatedDistance
getEstimatedDistance (Rental _) = Nothing

getFareProductType :: RideBooking -> DFareProduct.FareProductType
getFareProductType (OneWay _) = DFareProduct.ONE_WAY
getFareProductType (Rental _) = DFareProduct.RENTAL

instance (HasField x OneWayRideBooking a, HasField x RentalRideBooking a) => HasField x RideBooking a where
  hasField (OneWay rideBooking) = do
    let setter newField = OneWay $ fst (hasField @x rideBooking) newField
    (setter, snd (hasField @x rideBooking))
  hasField (Rental rideBooking) = do
    let setter newField = Rental $ fst (hasField @x rideBooking) newField
    (setter, snd (hasField @x rideBooking))
