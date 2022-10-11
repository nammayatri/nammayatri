{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Booking.Type where

import Beckn.Prelude hiding (first)
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.Quote (Quote)
import Domain.Types.Search (Search)
import Domain.Types.TransportStation (TransportStation)
import EulerHS.Prelude (first)
import Servant.API
import Tools.Auth

data BookingStatus = NEW | AWAITING_PAYMENT | CONFIRMED | CANCELLED
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema, Eq, ToParamSchema)

instance PrettyShow BookingStatus where
  prettyShow = prettyShow . Showable

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
    searchId :: Id Search,
    quoteId :: Id Quote,
    bknTxnId :: Text,
    requestorId :: Id Person,
    quantity :: Int,
    bppId :: Text,
    bppUrl :: BaseUrl,
    publicTransportSupportNumber :: Text,
    description :: Text,
    fare :: Money,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStationId :: Id TransportStation,
    arrivalStationId :: Id TransportStation,
    status :: BookingStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime
  }
  deriving (Generic, ToSchema, Show)
