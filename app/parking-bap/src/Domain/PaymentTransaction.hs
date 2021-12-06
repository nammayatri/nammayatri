{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.PaymentTransaction where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Booking.Type

data PaymentStatus = INITIALIZED | PENDING | FAILED | SUCCESS
  deriving (Generic, Show, Read, FromJSON, ToJSON)

data PaymentTransaction = PaymentTransaction
  { id :: Id PaymentTransaction,
    bookingId :: Id Booking,
    bknTxnId :: Text,
    paymentGatewayTxnId :: Text,
    fare :: Amount,
    status :: PaymentStatus,
    paymentUrl :: BaseUrl,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

data PaymentTransactionAPIEntity = PaymentTransactionAPIEntity
  { id :: Id PaymentTransaction,
    paymentGatewayTxnId :: Text,
    fare :: Amount,
    status :: PaymentStatus,
    paymentUrl :: BaseUrl,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON)

makePaymentTransactionAPIEntity :: PaymentTransaction -> PaymentTransactionAPIEntity
makePaymentTransactionAPIEntity PaymentTransaction {..} = PaymentTransactionAPIEntity {..}