{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Ticket where

import Data.Aeson
import Data.OpenApi
import Data.Time
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data TicketStatus
  = INIT
  | APPROVED
  | CONFIRMED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''TicketStatus)

data BPPTicket

data Ticket = Ticket
  { id :: Id Ticket,
    bppTicketId :: Maybe (Id BPPTicket),
    status :: TicketStatus,
    quoteId :: Maybe (Id DQuote.Quote),
    providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    paymentUrl :: Maybe Text,
    fulfillmentId :: Maybe Text,
    searchRequestId :: Text, -- transactionId
    bppOrderId :: Maybe Text,
    quantity :: Integer,
    pricePerAdult :: Money,
    totalPrice :: Money,
    fromLocation :: DLoc.Location,
    merchantId :: Id DMerchant.Merchant,
    qrData :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- API

data TicketAPIEntity = TicketAPIEntity
  { id :: Id Ticket,
    status :: TicketStatus,
    quoteId :: Maybe (Id DQuote.Quote),
    paymentUrl :: Maybe Text,
    quantity :: Integer,
    pricePerAdult :: Money,
    totalPrice :: Money,
    qrData :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
