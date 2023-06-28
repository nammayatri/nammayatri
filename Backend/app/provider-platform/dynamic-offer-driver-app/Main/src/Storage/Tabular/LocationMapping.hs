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

module Storage.Tabular.LocationMapping where

import qualified Domain.Types.LocationMapping as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Location as TLoc

derivePersistField "Domain.LocationMappingTags"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    LocationMappingT sql=location_mapping
      id Text
      locationId TLoc.LocationTId
      tag Domain.LocationMappingTags
      tagId Text
      order Int
      version Text
      Primary id
      deriving Generic
    |]

type FullLocationMappingT = (LocationMappingT, TLoc.LocationT)

instance TEntityKey LocationMappingT where
  type DomainKey LocationMappingT = Id Domain.LocationMapping
  fromKey (LocationMappingTKey _id) = Id _id
  toKey (Id id) = LocationMappingTKey id

instance FromTType FullLocationMappingT Domain.LocationMapping where
  fromTType (LocationMappingT {..}, locationT) = do
    location <- fromTType locationT

    return $
      Domain.LocationMapping
        { id = Id id,
          ..
        }

instance ToTType FullLocationMappingT Domain.LocationMapping where
  toTType Domain.LocationMapping {..} = do
    let locationMappingId = getId id
    let locationMappingT =
          LocationMappingT
            { id = locationMappingId,
              locationId = toKey location.id,
              ..
            }
    let locationT = toTType location
    (locationMappingT, locationT)
