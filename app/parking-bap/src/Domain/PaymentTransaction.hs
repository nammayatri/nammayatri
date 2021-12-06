{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.PaymentTransaction where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.App (BaseUrl)
import Beckn.Types.Id
import Domain.Booking

data PaymentStatus = INITIALIZED | PENDING | FAILED | SUCCESS
  deriving (Generic, Show, Read)

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
