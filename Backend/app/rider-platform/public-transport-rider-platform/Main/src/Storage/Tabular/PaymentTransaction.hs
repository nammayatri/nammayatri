{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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

instance FromTType PaymentTransactionT Domain.PaymentTransaction where
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

instance ToTType PaymentTransactionT Domain.PaymentTransaction where
  toTType Domain.PaymentTransaction {..} = do
    PaymentTransactionT
      { id = id.getId,
        bookingId = toKey bookingId,
        paymentUrl = showBaseUrl paymentUrl,
        fare = realToFrac fare,
        ..
      }
