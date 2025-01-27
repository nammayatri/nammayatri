{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SpecialZone where

import qualified Domain.Types.SpecialZone as Domain
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.Category"
derivePersistField "Domain.ShapeFile"
derivePersistField "LatLong"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SpecialZoneT sql=special_zone
      id Text
      name Text
      categoryCode Domain.Category
      geoJson Domain.ShapeFile
      geom Geom
      city Text
      state Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SpecialZoneT where
  type DomainKey SpecialZoneT = Id Domain.SpecialZone
  fromKey (SpecialZoneTKey _id) = Id _id
  toKey (Id id) = SpecialZoneTKey id

instance FromTType SpecialZoneT Domain.SpecialZone where
  fromTType SpecialZoneT {..} =
    return $
      Domain.SpecialZone
        { id = Id id,
          ..
        }

instance ToTType SpecialZoneT Domain.SpecialZone where
  toTType Domain.SpecialZone {..} =
    SpecialZoneT
      { id = getId id,
        geom = Geom,
        ..
      }
