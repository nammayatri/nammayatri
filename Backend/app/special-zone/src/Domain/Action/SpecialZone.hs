{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.SpecialZone where

import Data.Maybe (listToMaybe)
import qualified Domain.Types.SpecialZone as Domain
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps (LatLong)
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.EntryExit
import Storage.Queries.SpecialZone

lookupSpecialZone :: EsqDBFlow m r => LatLong -> m Domain.SpecialZone
lookupSpecialZone latLon = findSpecialZoneByLatLong latLon >>= fromMaybeM SpecialZoneNotFound

lookupSpecialZonesByRegion :: EsqDBFlow m r => LatLong -> LatLong -> m [Domain.SpecialZone]
lookupSpecialZonesByRegion minLatLng maxLatLng = do
  findSpecialZonesByRegion minLatLng maxLatLng

createSpecialZone :: EsqDBFlow m r => Domain.SpecialZoneAPIEntity -> m APISuccess
createSpecialZone specialZoneReq = do
  (mbMultiPolygon, points) <- mkSzWithGates specialZoneReq
  (specialZone, geometry) <- case mbMultiPolygon of
    Nothing -> throwError SpecialZoneNotFound
    Just multiPolygon_ -> do
      specialZone <- mkSpecialZone specialZoneReq multiPolygon_
      pure (specialZone, multiPolygon_.geometry)
  entryExits <- mapM (mkEntryExit specialZone.id) points
  runTransaction $ do
    create specialZone geometry
    createMany entryExits
  pure Success

updateSpecialZone :: EsqDBFlow m r => Domain.SpecialZone -> m APISuccess
updateSpecialZone specialZone = do
  features <- case specialZone.geoJson of
    Domain.ShapeFile sdt -> pure sdt.features
  let (mbMultiPolygon, points) = mkPolygonWithGates features
  geometry <- case mbMultiPolygon of
    Nothing -> throwError SpecialZoneNotFound
    Just multiPolygon_ -> pure multiPolygon_.geometry
  entryExits <- mapM (mkEntryExit specialZone.id) points
  runTransaction $ do
    update specialZone geometry
    deleteBySpecialZoneId specialZone.id
    createMany entryExits
  pure Success

deleteSpecialZone :: EsqDBFlow m r => Id Domain.SpecialZone -> m APISuccess
deleteSpecialZone specialZoneId = do
  runTransaction $ do
    deleteBySpecialZoneId specialZoneId
    deleteById specialZoneId
  pure Success

mkPolygonWithGates :: [Domain.LocationFeature] -> (Maybe Domain.LocationFeature, [Domain.LocationFeature])
mkPolygonWithGates features = do
  let multiPolygons = filter (\feats -> feats.geometry._type == Domain.MultiPolygon || feats.geometry._type == Domain.Polygon) features
      points = filter (\feats -> feats.geometry._type == Domain.Point) features
  (listToMaybe multiPolygons, points)

mkSzWithGates :: MonadFlow m => Domain.SpecialZoneAPIEntity -> m (Maybe Domain.LocationFeature, [Domain.LocationFeature])
mkSzWithGates specialZoneReq = do
  features <- case specialZoneReq.geoJson of
    Domain.ShapeFile sdt -> pure sdt.features
  return $ mkPolygonWithGates features

mkEntryExit :: MonadFlow m => Id Domain.SpecialZone -> Domain.LocationFeature -> m Domain.EntryExit
mkEntryExit specialZoneId feat = do
  id_ <- generateGUID
  now <- getCurrentTime
  (lat, lon) <- case feat.geometry.coordinates of
    Domain.PointCoords [lat, lon] -> pure (lat, lon)
    _ -> throwError PointNotFound
  _type <- case feat.properties._type of
    Nothing -> throwError PointNotFound
    Just _type -> pure _type
  return $
    Domain.EntryExit
      { id = Id id_,
        area = feat.properties.area,
        address = feat.properties.address,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkSpecialZone :: MonadFlow m => Domain.SpecialZoneAPIEntity -> Domain.LocationFeature -> m Domain.SpecialZone
mkSpecialZone Domain.SpecialZoneAPIEntity {..} feat = do
  id_ <- generateGUID
  now <- getCurrentTime
  return $
    Domain.SpecialZone
      { id = Id id_,
        createdAt = now,
        updatedAt = now,
        geoJson =
          Domain.ShapeFile
            Domain.ShapeFileType
              { _type = "FeatureCollection",
                features = [feat]
              },
        ..
      }
