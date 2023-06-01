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

module Storage.Tabular.CallbackRequest where

import qualified Domain.Types.CallbackRequest as Domain
import Kernel.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Merchant as DM

derivePersistField "Domain.CallbackRequestStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CallbackRequestT sql=callback_request
      id Text
      merchantId DM.MerchantTId
      customerName Text Maybe
      customerPhoneEncrypted Text
      customerPhoneHash DbHash
      customerMobileCountryCode Text
      status Domain.CallbackRequestStatus
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey CallbackRequestT where
  type DomainKey CallbackRequestT = Id Domain.CallbackRequest
  fromKey (CallbackRequestTKey _id) = Id _id
  toKey (Id id) = CallbackRequestTKey id

instance FromTType CallbackRequestT Domain.CallbackRequest where
  fromTType CallbackRequestT {..} = do
    return $
      Domain.CallbackRequest
        { id = Id id,
          merchantId = fromKey merchantId,
          customerPhone = EncryptedHashed (Encrypted customerPhoneEncrypted) customerPhoneHash,
          ..
        }

instance ToTType CallbackRequestT Domain.CallbackRequest where
  toTType Domain.CallbackRequest {..} =
    CallbackRequestT
      { id = getId id,
        merchantId = toKey merchantId,
        customerPhoneEncrypted = customerPhone & unEncrypted . (.encrypted),
        customerPhoneHash = customerPhone & (.hash),
        ..
      }
