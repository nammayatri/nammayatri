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

import qualified Domain.Types.LocationAddress as DL
import Domain.Types.SearchRequest.SearchReqLocation as DSRL
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest.SearchReqLocation as BeamSRL

create :: MonadFlow m => SearchReqLocation -> m ()
create = createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchReqLocation -> m (Maybe SearchReqLocation)
findById (Id searchReqLocationId) = findOneWithKV [Se.Is BeamSRL.id $ Se.Eq searchReqLocationId]

instance FromTType' BeamSRL.SearchReqLocation SearchReqLocation where
  fromTType' BeamSRL.SearchReqLocationT {..} = do
    let address = DL.LocationAddress {..}
    pure $
      Just
        SearchReqLocation
          { id = Id id,
            lat = lat,
            lon = lon,
            address = address,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' BeamSRL.SearchReqLocation SearchReqLocation where
  toTType' SearchReqLocation {..} = do
    BeamSRL.SearchReqLocationT
      { BeamSRL.id = getId id,
        BeamSRL.lat = lat,
        BeamSRL.lon = lon,
        BeamSRL.street = DL.street address,
        BeamSRL.door = DL.door address,
        BeamSRL.city = DL.city address,
        BeamSRL.state = DL.state address,
        BeamSRL.country = DL.country address,
        BeamSRL.building = DL.building address,
        BeamSRL.areaCode = DL.areaCode address,
        BeamSRL.area = DL.area address,
        BeamSRL.ward = DL.ward address,
        BeamSRL.placeId = DL.placeId address,
        BeamSRL.createdAt = createdAt,
        BeamSRL.updatedAt = updatedAt
      }
