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

module Storage.Tabular.DriverLocation where

import qualified Domain.Types.DriverLocation as Domain
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverLocationT sql=driver_location
      driverId PersonTId
      lat Double
      lon Double
      point Point
      coordinatesCalculatedAt UTCTime
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverLocationT where
  type DomainKey DriverLocationT = Id Person
  fromKey (DriverLocationTKey _id) = fromKey _id
  toKey id = DriverLocationTKey $ toKey id

instance FromTType DriverLocationT Domain.DriverLocation where
  fromTType DriverLocationT {..} = do
    return $
      Domain.DriverLocation
        { driverId = fromKey driverId,
          ..
        }

instance ToTType DriverLocationT Domain.DriverLocation where
  toTType Domain.DriverLocation {..} =
    DriverLocationT
      { driverId = toKey driverId,
        point = Point,
        ..
      }
