{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Maps.PlaceNameCache
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Maps.PlaceNameCache
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Maps.PlaceNameCache as BeamPNC

create :: MonadFlow m => PlaceNameCache -> m ()
create = createWithKV

findPlaceByPlaceId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [PlaceNameCache]
findPlaceByPlaceId placeId = findAllWithKV [Se.Is BeamPNC.placeId $ Se.Eq (Just placeId)]

findPlaceByGeoHash :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [PlaceNameCache]
findPlaceByGeoHash geoHash = findAllWithKV [Se.Is BeamPNC.geoHash $ Se.Eq (Just geoHash)]

instance FromTType' BeamPNC.PlaceNameCache PlaceNameCache where
  fromTType' BeamPNC.PlaceNameCacheT {..} = do
    pure $
      Just
        PlaceNameCache
          { id = Id id,
            formattedAddress = formattedAddress,
            plusCode = plusCode,
            lat = lat,
            lon = lon,
            placeId = placeId,
            addressComponents = addressComponents,
            geoHash = geoHash
          }

instance ToTType' BeamPNC.PlaceNameCache PlaceNameCache where
  toTType' PlaceNameCache {..} = do
    BeamPNC.PlaceNameCacheT
      { BeamPNC.id = getId id,
        BeamPNC.formattedAddress = formattedAddress,
        BeamPNC.plusCode = plusCode,
        BeamPNC.lat = lat,
        BeamPNC.lon = lon,
        BeamPNC.placeId = placeId,
        BeamPNC.addressComponents = addressComponents,
        BeamPNC.geoHash = geoHash
      }
