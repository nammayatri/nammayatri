{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Booking.Type where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.PublicTranport (PublicTranport)
import Domain.Quote (Quote)
import Domain.Search (Search)
import Tools.Auth

data BookingStatus = NEW | AWAITING_PAYMENT | CONFIRMED | CANCELLED
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema)

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
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStationId :: Id PublicTranport,
    arrivalStationId :: Id PublicTranport,
    status :: BookingStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime
  }
  deriving (Generic, ToSchema)
