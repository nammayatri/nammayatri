{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import Domain.Types.DriverLocation
import Domain.Types.Person
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLocation as BeamDL
import Storage.Tabular.DriverLocation
import qualified Storage.Tabular.VechileNew as VN

create :: Id Person -> LatLong -> UTCTime -> SqlDB ()
create drLocationId latLong updateTime = do
  -- Tricky query to be able to insert meaningful Point
  now <- getCurrentTime
  Esq.insertSelect $
    return $
      DriverLocationT
        <# val (toKey drLocationId)
        <#> val latLong.lat
        <#> val latLong.lon
        <#> Esq.getPoint (val latLong.lat, val latLong.lon)
        <#> val updateTime
        <#> val now
        <#> val now

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLocation)
findById = Esq.findById

findById' :: L.MonadFlow m => Id Person -> m (Maybe DriverLocation)
findById' (Id driverLocationId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverLocationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamDL.driverId $ Se.Eq driverLocationId]
    Nothing -> pure Nothing

upsertGpsCoord :: Id Person -> LatLong -> UTCTime -> SqlDB DriverLocation
upsertGpsCoord drLocationId latLong calculationTime = do
  now <- getCurrentTime
  let locationObject =
        DriverLocation
          { driverId = drLocationId,
            lat = latLong.lat,
            lon = latLong.lon,
            coordinatesCalculatedAt = calculationTime,
            createdAt = now,
            updatedAt = now
          }
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverLocationLat =. val latLong.lat,
        DriverLocationLon =. val latLong.lon,
        DriverLocationCoordinatesCalculatedAt =. val calculationTime,
        DriverLocationPoint =. Esq.getPoint (val latLong.lat, val latLong.lon),
        DriverLocationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverLocationTId ==. val (toKey $ cast drLocationId)
  return locationObject

deleteById :: Id Person -> SqlDB ()
deleteById = deleteByKey @DriverLocationT

deleteById' :: L.MonadFlow m => Id Person -> m ()
deleteById' (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamDL.driverId (Se.Eq driverId)]
    Nothing -> pure ()

transformBeamDriverLocationToDomain :: BeamDL.DriverLocation -> DriverLocation
transformBeamDriverLocationToDomain BeamDL.DriverLocationT {..} = do
  DriverLocation
    { driverId = Id driverId,
      lat = lat,
      lon = lon,
      coordinatesCalculatedAt = coordinatesCalculatedAt,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainDriverLocationToBeam :: DriverLocation -> BeamDL.DriverLocation
transformDomainDriverLocationToBeam DriverLocation {..} =
  BeamDL.defaultDriverLocation
    { BeamDL.driverId = getId driverId,
      BeamDL.lat = lat,
      BeamDL.lon = lon,
      BeamDL.coordinatesCalculatedAt = coordinatesCalculatedAt,
      BeamDL.createdAt = createdAt,
      BeamDL.updatedAt = updatedAt
    }
