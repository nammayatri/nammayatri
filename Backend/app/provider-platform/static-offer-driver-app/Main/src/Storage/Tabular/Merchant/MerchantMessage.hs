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

module Storage.Tabular.Merchant.MerchantMessage where

import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.MerchantMessage as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.MessageKey"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantMessageT sql=merchant_message
      merchantId MerchantTId
      messageKey Domain.MessageKey
      message Text
      updatedAt UTCTime
      createdAt UTCTime
      Primary merchantId messageKey
      deriving Generic
    |]

instance TEntityKey MerchantMessageT where
  type DomainKey MerchantMessageT = (Id Domain.Merchant, Domain.MessageKey)
  fromKey (MerchantMessageTKey _id messageKey) = (fromKey _id, messageKey)
  toKey (id, messageKey) = MerchantMessageTKey (toKey id) messageKey

instance FromTType MerchantMessageT Domain.MerchantMessage where
  fromTType MerchantMessageT {..} = do
    return $
      Domain.MerchantMessage
        { merchantId = fromKey merchantId,
          ..
        }

instance ToTType MerchantMessageT Domain.MerchantMessage where
  toTType Domain.MerchantMessage {..} = do
    MerchantMessageT
      { merchantId = toKey merchantId,
        ..
      }
