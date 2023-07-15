{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.SearchRequest.SearchReqLocation where

import qualified Domain.Types.LocationAddress as DL
import Domain.Types.SearchRequest.SearchReqLocation as DSRL
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest.SearchReqLocation as BeamSRL

create :: L.MonadFlow m => SearchReqLocation -> m (MeshResult ())
create bl = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRL.SearchReqLocationT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainSearchReqLocationToBeam bl)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id SearchReqLocation -> m (Maybe SearchReqLocation)
findById (Id searchReqLocationId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSRL.SearchReqLocationT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamSearchReqLocationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamSRL.id $ Se.Eq searchReqLocationId]
    Nothing -> pure Nothing

transformBeamSearchReqLocationToDomain :: BeamSRL.SearchReqLocation -> SearchReqLocation
transformBeamSearchReqLocationToDomain BeamSRL.SearchReqLocationT {..} = do
  let address = DL.LocationAddress {..}
  SearchReqLocation
    { id = Id id,
      lat = lat,
      lon = lon,
      address = address,
      updatedAt = updatedAt,
      createdAt = createdAt
    }

transformDomainSearchReqLocationToBeam :: SearchReqLocation -> BeamSRL.SearchReqLocation
transformDomainSearchReqLocationToBeam SearchReqLocation {..} =
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
