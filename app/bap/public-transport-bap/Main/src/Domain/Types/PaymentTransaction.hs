{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.PaymentTransaction where

import Domain.Types.Booking.Type
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data PaymentStatus = PENDING | FAILED | SUCCESS
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema, Eq)

instance PrettyShow PaymentStatus where
  prettyShow = prettyShow . Showable

data PaymentTransaction = PaymentTransaction
  { id :: Id PaymentTransaction,
    bookingId :: Id Booking,
    bknTxnId :: Text,
    paymentGatewayTxnId :: Text,
    paymentGatewayTxnStatus :: Text,
    fare :: Money,
    status :: PaymentStatus,
    paymentUrl :: BaseUrl,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

data PaymentTransactionAPIEntity = PaymentTransactionAPIEntity
  { id :: Id PaymentTransaction,
    paymentGatewayTxnId :: Text,
    fare :: Money,
    status :: PaymentStatus,
    paymentUrl :: BaseUrl,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

makePaymentTransactionAPIEntity :: PaymentTransaction -> PaymentTransactionAPIEntity
makePaymentTransactionAPIEntity PaymentTransaction {..} = PaymentTransactionAPIEntity {..}
