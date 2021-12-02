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
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.TH
import qualified Domain.PaymentTransaction as Domain
import Servant.Client
import Storage.Tabular.Booking (BookingTId)

derivePersistField "Domain.PaymentStatus"

share
  [mkPersist defaultSqlSettings]
  [defaultQQ|
    PaymentTransactionT sql=payment_transaction
      id Text
      bookingId BookingTId
      bknTxnId Text
      paymentGatewayTxnId Text
      fare Amount
      status Domain.PaymentStatus
      paymentUrl Text
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PaymentTransactionT Domain.PaymentTransaction where
  fromKey (PaymentTransactionTKey _id) = Id _id
  toKey id = PaymentTransactionTKey id.getId

instance TEntity PaymentTransactionT Domain.PaymentTransaction where
  fromTEntity entity = do
    let (PaymentTransactionTKey _id) = entityKey entity
        PaymentTransactionT {..} = entityVal entity
    paymentUrl_ <- parseBaseUrl $ T.unpack paymentUrl
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
        paymentUrl = T.pack $ showBaseUrl paymentUrl,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
