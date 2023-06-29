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

module Storage.Tabular.Driver.GoHomeFeature.DriverHomeLocation where

import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverHomeLocationT sql=driver_home_location
      id Text
      driverId PersonTId
      lat Double
      lon Double
      createdAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey DriverHomeLocationT where
  type DomainKey DriverHomeLocationT = Id Domain.DriverHomeLocation
  fromKey (DriverHomeLocationTKey _id) = Id _id
  toKey id = DriverHomeLocationTKey $ getId id

instance FromTType DriverHomeLocationT Domain.DriverHomeLocation where
  fromTType DriverHomeLocationT {..} = do
    return $
      Domain.DriverHomeLocation
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }

instance ToTType DriverHomeLocationT Domain.DriverHomeLocation where
  toTType Domain.DriverHomeLocation {..} =
    DriverHomeLocationT
      { id = id.getId,
        driverId = toKey driverId,
        ..
      }
