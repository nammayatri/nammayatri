{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Domain.PaymentTransaction
  ( module Storage.Domain.PaymentTransaction,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.App (BaseUrl)
import Beckn.Types.Id
import qualified Data.Text as T
import Data.Time (UTCTime)
import Servant.Client (parseBaseUrl, showBaseUrl)
import Storage.Domain.Booking
import Storage.Tabular.PaymentTransaction
import Storage.Tabular.PaymentTransaction as Reexport (PaymentTransactionT)
import Storage.Tabular.PaymentTransaction as Reexport hiding (PaymentTransactionT (..))

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

instance TEntityKey PaymentTransactionT PaymentTransaction where
  fromKey (PaymentTransactionTKey _id) = Id _id
  toKey PaymentTransaction {id} = PaymentTransactionTKey id.getId

instance TEntity PaymentTransactionT PaymentTransaction where
  fromTEntity entity = do
    let (PaymentTransactionTKey _id) = entityKey entity
        PaymentTransactionT {..} = entityVal entity
    paymentUrl <- parseBaseUrl $ T.unpack paymentTransactionTPaymentUrl
    return $
      PaymentTransaction
        { id = Id _id,
          bookingId = fromKey paymentTransactionTBookingId,
          bknTxnId = paymentTransactionTBknTxnId,
          paymentGatewayTxnId = paymentTransactionTPaymentGatewayTxnId,
          fare = paymentTransactionTFare,
          status = paymentTransactionTStatus,
          paymentUrl = paymentUrl,
          updatedAt = paymentTransactionTUpdatedAt,
          createdAt = paymentTransactionTCreatedAt
        }
  toTType PaymentTransaction {..} = do
    PaymentTransactionT
      { paymentTransactionTBookingId = BookingTKey id.getId,
        paymentTransactionTBknTxnId = bknTxnId,
        paymentTransactionTPaymentGatewayTxnId = paymentGatewayTxnId,
        paymentTransactionTFare = fare,
        paymentTransactionTStatus = status,
        paymentTransactionTPaymentUrl = T.pack $ showBaseUrl paymentUrl,
        paymentTransactionTUpdatedAt = updatedAt,
        paymentTransactionTCreatedAt = createdAt
      }
  toTEntity a = do
    Entity (toKey a) $ toTType a
