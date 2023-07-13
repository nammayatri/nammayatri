{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Maps.PlaceNameCache
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Maps.PlaceNameCache
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Maps.PlaceNameCache as BeamMapsPNC
import qualified Storage.Beam.Maps.PlaceNameCache as BeamPNC

create :: L.MonadFlow m => PlaceNameCache -> m (MeshResult ())
create placeNameCache = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPNC.PlaceNameCacheT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainPlaceNameCacheToBeam placeNameCache)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findPlaceByPlaceId :: Transactionable m => Text -> m [PlaceNameCache]
-- findPlaceByPlaceId placeId =
--   Esq.findAll $ do
--     placeNameCache <- from $ table @PlaceNameCacheT
--     where_ $ placeNameCache ^. PlaceNameCachePlaceId ==. val (Just placeId)
--     return placeNameCache

findPlaceByPlaceId :: L.MonadFlow m => Text -> m [PlaceNameCache]
findPlaceByPlaceId placeId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamMapsPNC.PlaceNameCacheT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamPlaceNameCacheToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamMapsPNC.placeId $ Se.Eq (Just placeId)]
    Nothing -> pure []

-- findPlaceByGeoHash :: Transactionable m => Text -> m [PlaceNameCache]
-- findPlaceByGeoHash geoHash =
--   Esq.findAll $ do
--     placeNameCache <- from $ table @PlaceNameCacheT
--     where_ $ placeNameCache ^. PlaceNameCacheGeoHash ==. val (Just geoHash)
--     return placeNameCache

findPlaceByGeoHash :: L.MonadFlow m => Text -> m [PlaceNameCache]
findPlaceByGeoHash geoHash = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamMapsPNC.PlaceNameCacheT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamPlaceNameCacheToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamMapsPNC.geoHash $ Se.Eq (Just geoHash)]
    Nothing -> pure []

transformBeamPlaceNameCacheToDomain :: BeamPNC.PlaceNameCache -> PlaceNameCache
transformBeamPlaceNameCacheToDomain BeamPNC.PlaceNameCacheT {..} = do
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

transformDomainPlaceNameCacheToBeam :: PlaceNameCache -> BeamPNC.PlaceNameCache
transformDomainPlaceNameCacheToBeam PlaceNameCache {..} =
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
