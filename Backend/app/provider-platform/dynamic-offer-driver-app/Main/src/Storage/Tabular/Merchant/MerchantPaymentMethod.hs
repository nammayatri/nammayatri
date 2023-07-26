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

module Storage.Tabular.Merchant.MerchantPaymentMethod where

import qualified Domain.Types.Merchant.MerchantPaymentMethod as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.PaymentType"
derivePersistField "Domain.PaymentInstrument"
derivePersistField "Domain.PaymentCollector"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantPaymentMethodT sql=merchant_payment_method
      id Text
      merchantId MerchantTId
      paymentType Domain.PaymentType
      paymentInstrument Domain.PaymentInstrument
      collectedBy Domain.PaymentCollector
      priority Int
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey MerchantPaymentMethodT where
  type DomainKey MerchantPaymentMethodT = Id Domain.MerchantPaymentMethod
  fromKey (MerchantPaymentMethodTKey _id) = Id _id
  toKey (Id id) = MerchantPaymentMethodTKey id

instance FromTType MerchantPaymentMethodT Domain.MerchantPaymentMethod where
  fromTType MerchantPaymentMethodT {..} = do
    return $
      Domain.MerchantPaymentMethod
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType MerchantPaymentMethodT Domain.MerchantPaymentMethod where
  toTType Domain.MerchantPaymentMethod {..} = do
    MerchantPaymentMethodT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
