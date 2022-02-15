{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.PaymentTransaction where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Booking.Type

data PaymentStatus = PENDING | FAILED | SUCCESS
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema)

data PaymentTransaction = PaymentTransaction
  { id :: Id PaymentTransaction,
    bookingId :: Id Booking,
    bknTxnId :: Text,
    paymentGatewayTxnId :: Text,
    paymentGatewayTxnStatus :: Text,
    fare :: Amount,
    status :: PaymentStatus,
    paymentUrl :: BaseUrl,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

data PaymentTransactionAPIEntity = PaymentTransactionAPIEntity
  { id :: Id PaymentTransaction,
    paymentGatewayTxnId :: Text,
    fare :: Amount,
    status :: PaymentStatus,
    paymentUrl :: BaseUrl,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

makePaymentTransactionAPIEntity :: PaymentTransaction -> PaymentTransactionAPIEntity
makePaymentTransactionAPIEntity PaymentTransaction {..} = PaymentTransactionAPIEntity {..}
