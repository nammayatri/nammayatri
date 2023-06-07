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

import Domain.Types.SearchRequest.SearchReqLocation
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest.SearchReqLocation as BeamSRL

create :: L.MonadFlow m => SearchReqLocation -> m (MeshResult ())
create bl = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainSearchReqLocationToBeam bl)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id SearchReqLocation -> m (Maybe SearchReqLocation)
findById (Id searchReqLocationId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamSearchReqLocationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamSRL.id $ Se.Eq searchReqLocationId]
    Nothing -> pure Nothing

transformBeamSearchReqLocationToDomain :: BeamSRL.SearchReqLocation -> SearchReqLocation
transformBeamSearchReqLocationToDomain BeamSRL.SearchReqLocationT {..} = do
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
      area = area,
      full_address = full_address,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainSearchReqLocationToBeam :: SearchReqLocation -> BeamSRL.SearchReqLocation
transformDomainSearchReqLocationToBeam SearchReqLocation {..} =
  BeamSRL.defaultSearchReqLocation
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
