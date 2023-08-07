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

module Storage.Tabular.MetaData where

import qualified Domain.Types.MetaData as Domain
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MetaDataT sql=meta_data
      driverId PersonTId
      device Text Maybe
      deviceOS Text Maybe
      deviceDateTime UTCTime Maybe
      appPermissions Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey MetaDataT where
  type DomainKey MetaDataT = Id Person
  fromKey (MetaDataTKey _id) = fromKey _id
  toKey id = MetaDataTKey $ toKey id

instance FromTType MetaDataT Domain.MetaData where
  fromTType MetaDataT {..} = do
    return $
      Domain.MetaData
        { driverId = fromKey driverId,
          ..
        }

instance ToTType MetaDataT Domain.MetaData where
  toTType Domain.MetaData {..} =
    MetaDataT
      { driverId = toKey driverId,
        ..
      }
