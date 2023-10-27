{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequest.SearchReqLocation where

import Domain.Types.SearchRequest.SearchReqLocation
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest.SearchReqLocation as BeamSRL

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SearchReqLocation -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchReqLocation -> m (Maybe SearchReqLocation)
findById (Id searchReqLocationId) = findOneWithKV [Se.Is BeamSRL.id $ Se.Eq searchReqLocationId]

instance FromTType' BeamSRL.SearchReqLocation SearchReqLocation where
  fromTType' BeamSRL.SearchReqLocationT {..} = do
    pure $
      Just
        SearchReqLocation
          { id = Id id,
            lat = lat,
            lon = lon,
            street = street,
            door = door,
            city = city,
            state = state,
            country = country,
            building = building,
            areaCode = areaCode,
            full_address = full_address,
            area = area,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamSRL.SearchReqLocation SearchReqLocation where
  toTType' SearchReqLocation {..} = do
    BeamSRL.SearchReqLocationT
      { BeamSRL.id = getId id,
        BeamSRL.lat = lat,
        BeamSRL.lon = lon,
        BeamSRL.street = street,
        BeamSRL.door = door,
        BeamSRL.city = city,
        BeamSRL.state = state,
        BeamSRL.country = country,
        BeamSRL.building = building,
        BeamSRL.areaCode = areaCode,
        BeamSRL.area = area,
        BeamSRL.full_address = full_address,
        BeamSRL.createdAt = createdAt,
        BeamSRL.updatedAt = updatedAt
      }
