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

module Storage.Tabular.RegistrationToken where

import qualified Domain.Types.RegistrationToken as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.App (RegToken)
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RegistrationTokenT sql=registration_token
      id Text
      token RegToken
      personId PersonTId
      merchantId MerchantTId
      createdAt UTCTime
      operatingCity City
      Primary id
      deriving Generic
    |]

instance TEntityKey RegistrationTokenT where
  type DomainKey RegistrationTokenT = Id Domain.RegistrationToken
  fromKey (RegistrationTokenTKey _id) = Id _id
  toKey (Id id) = RegistrationTokenTKey id

instance FromTType RegistrationTokenT Domain.RegistrationToken where
  fromTType RegistrationTokenT {..} = do
    return $
      Domain.RegistrationToken
        { id = Id id,
          personId = fromKey personId,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType RegistrationTokenT Domain.RegistrationToken where
  toTType Domain.RegistrationToken {..} =
    RegistrationTokenT
      { id = getId id,
        personId = toKey personId,
        merchantId = toKey merchantId,
        ..
      }
