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

module Storage.Tabular.Vehicle where

import qualified Domain.Types.Person as DPers
import qualified Domain.Types.Vehicle as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Merchant as TM
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.Category"
derivePersistField "Domain.Variant"
derivePersistField "Domain.EnergyType"
derivePersistField "Domain.RegistrationCategory"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    VehicleT sql=vehicle
      driverId PersonTId
      merchantId TM.MerchantTId
      variant Domain.Variant
      model Text
      color Text
      registrationNo Text
      capacity Int Maybe
      category Domain.Category Maybe
      make Text Maybe
      size Text Maybe
      energyType Domain.EnergyType Maybe
      registrationCategory Domain.RegistrationCategory Maybe
      vehicleClass Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      Unique VehicleRegistrationNo
      deriving Generic
    |]

instance TEntityKey VehicleT where
  type DomainKey VehicleT = Id DPers.Person
  fromKey (VehicleTKey _id) = fromKey _id
  toKey id = VehicleTKey $ toKey id

instance FromTType VehicleT Domain.Vehicle where
  fromTType VehicleT {..} = do
    return $
      Domain.Vehicle
        { driverId = fromKey driverId,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType VehicleT Domain.Vehicle where
  toTType Domain.Vehicle {..} =
    VehicleT
      { driverId = toKey driverId,
        merchantId = toKey merchantId,
        ..
      }
