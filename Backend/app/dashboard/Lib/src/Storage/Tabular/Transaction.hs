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

module Storage.Tabular.Transaction where

import qualified Domain.Types.ServerName as DSN
import qualified Domain.Types.Transaction as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.Endpoint"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransactionT sql=transaction
      id Text
      requestorId PersonTId Maybe
      serverName DSN.ServerName Maybe
      merchantId MerchantTId Maybe
      commonDriverId Text Maybe
      commonRideId Text Maybe
      endpoint Domain.Endpoint
      request Text Maybe
      response Text Maybe
      responseError Text Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey TransactionT where
  type DomainKey TransactionT = Id Domain.Transaction
  fromKey (TransactionTKey _id) = Id _id
  toKey (Id id) = TransactionTKey id

instance FromTType TransactionT Domain.Transaction where
  fromTType TransactionT {..} = do
    return $
      Domain.Transaction
        { id = Id id,
          requestorId = fromKey <$> requestorId,
          merchantId = fromKey <$> merchantId,
          commonDriverId = Id <$> commonDriverId,
          commonRideId = Id <$> commonRideId,
          ..
        }

instance ToTType TransactionT Domain.Transaction where
  toTType Domain.Transaction {..} =
    TransactionT
      { id = getId id,
        requestorId = toKey <$> requestorId,
        merchantId = toKey <$> merchantId,
        commonDriverId = getId <$> commonDriverId,
        commonRideId = getId <$> commonRideId,
        ..
      }
