{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.PaymentTransaction where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.PaymentTransaction as Domain
import Storage.Tabular.Booking (BookingTId)

derivePersistField "Domain.PaymentStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PaymentTransactionT sql=payment_transaction
      id Text
      bookingId BookingTId
      bknTxnId Text
      paymentGatewayTxnId Text
      paymentGatewayTxnStatus Text
      fare Amount
      status Domain.PaymentStatus
      paymentUrl Text
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PaymentTransactionT where
  type DomainKey PaymentTransactionT = Id Domain.PaymentTransaction
  fromKey (PaymentTransactionTKey _id) = Id _id
  toKey id = PaymentTransactionTKey id.getId

instance TEntity PaymentTransactionT Domain.PaymentTransaction where
  fromTEntity entity = do
    let (PaymentTransactionTKey _id) = entityKey entity
        PaymentTransactionT {..} = entityVal entity
    paymentUrl_ <- parseBaseUrl paymentUrl
    return $
      Domain.PaymentTransaction
        { id = Id _id,
          bookingId = fromKey bookingId,
          paymentUrl = paymentUrl_,
          ..
        }
  toTType Domain.PaymentTransaction {..} = do
    PaymentTransactionT
      { id = id.getId,
        bookingId = toKey bookingId,
        paymentUrl = showBaseUrl paymentUrl,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
