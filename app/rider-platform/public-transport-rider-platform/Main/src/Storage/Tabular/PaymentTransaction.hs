{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.PaymentTransaction where

import qualified Domain.Types.PaymentTransaction as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
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
      fare HighPrecMoney
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

instance TType PaymentTransactionT Domain.PaymentTransaction where
  fromTType PaymentTransactionT {..} = do
    paymentUrl_ <- parseBaseUrl paymentUrl
    return $
      Domain.PaymentTransaction
        { id = Id id,
          bookingId = fromKey bookingId,
          paymentUrl = paymentUrl_,
          fare = roundToIntegral fare,
          ..
        }
  toTType Domain.PaymentTransaction {..} = do
    PaymentTransactionT
      { id = id.getId,
        bookingId = toKey bookingId,
        paymentUrl = showBaseUrl paymentUrl,
        fare = realToFrac fare,
        ..
      }
