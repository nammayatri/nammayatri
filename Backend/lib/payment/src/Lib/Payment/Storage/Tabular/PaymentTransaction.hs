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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Tabular.PaymentTransaction where

import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentTransaction as Domain
import qualified Lib.Payment.Storage.Tabular.PaymentOrder as TOrder

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PaymentTransactionT sql=payment_transaction
      id Text
      txnUUID Text Maybe sql=txn_uuid
      paymentMethodType Text Maybe
      paymentMethod Text Maybe
      respMessage Text Maybe
      respCode Text Maybe
      gatewayReferenceId Text Maybe
      orderId TOrder.PaymentOrderTId
      merchantId Text -- TM.MerchantTId
      amount HighPrecMoney
      currency Payment.Currency
      dateCreated UTCTime Maybe
      statusId Int
      status Payment.TransactionStatus
      juspayResponse Text Maybe
      mandateStatus Payment.MandateStatus Maybe
      mandateStartDate UTCTime Maybe
      mandateEndDate UTCTime Maybe
      mandateId Text Maybe
      mandateFrequency Payment.MandateFrequency Maybe
      mandateMaxAmount HighPrecMoney Maybe
      bankErrorCode Text Maybe
      bankErrorMessage Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PaymentTransactionT where
  type DomainKey PaymentTransactionT = Id Domain.PaymentTransaction
  fromKey (PaymentTransactionTKey _id) = Id _id
  toKey (Id id) = PaymentTransactionTKey id

instance FromTType PaymentTransactionT Domain.PaymentTransaction where
  fromTType PaymentTransactionT {..} = do
    return $
      Domain.PaymentTransaction
        { id = Id id,
          orderId = fromKey orderId,
          merchantId = Id merchantId,
          ..
        }

instance ToTType PaymentTransactionT Domain.PaymentTransaction where
  toTType Domain.PaymentTransaction {..} =
    PaymentTransactionT
      { id = getId id,
        orderId = toKey orderId,
        merchantId = merchantId.getId,
        ..
      }
