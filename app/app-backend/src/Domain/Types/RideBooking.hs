{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideBooking where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequest as DSearchRequest
import Servant.API

data RideBookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPRideBooking

data RideBooking = RideBooking
  { id :: Id RideBooking,
    bppBookingId :: Maybe (Id BPPRideBooking),
    requestId :: Id DSearchRequest.SearchRequest,
    quoteId :: Id DQuote.Quote,
    status :: RideBookingStatus,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    startTime :: UTCTime,
    riderId :: Id DPerson.Person,
    fromLocationId :: Id DLoc.SearchReqLocation,
    toLocationId :: Maybe (Id DLoc.SearchReqLocation),
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    distance :: Maybe Double,
    vehicleVariant :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)
