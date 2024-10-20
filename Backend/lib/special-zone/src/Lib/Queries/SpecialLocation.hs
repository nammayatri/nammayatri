{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.SpecialLocation where

import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.Functions as F
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import Lib.Tabular.SpecialLocation
import qualified Lib.Types.GateInfo as GD
import qualified Lib.Types.SpecialLocation as D

data SpecialLocationFull = SpecialLocationFull
  { id :: Id D.SpecialLocation,
    locationName :: Text,
    category :: Text,
    merchantOperatingCityId :: Maybe Text,
    gatesInfo :: [GD.GateInfoFull],
    gates :: [D.GatesInfo], --TODO: deprecate this later
    geoJson :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

findById :: Transactionable m => Id D.SpecialLocation -> m (Maybe D.SpecialLocation)
findById = Esq.findById

findByIdWithGeom :: Transactionable m => Id D.SpecialLocation -> m (Maybe (D.SpecialLocation, Maybe Text))
findByIdWithGeom specialLocationId =
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $ specialLocation ^. SpecialLocationId ==. val specialLocationId.getId
    return (specialLocation, F.mbGetGeomGeoJSON)

filterGates :: Maybe SpecialLocationFull -> Bool -> Maybe SpecialLocationFull
filterGates (Just specialLocBody) isOrigin =
  let gateTypeRequired = if isOrigin then GD.Pickup else GD.Drop
      filteredGates = filter matchGateType (gatesInfo specialLocBody)
      matchGateType gate = case gate of
        GD.GateInfoFull {GD.gateType = gt} -> gt == gateTypeRequired
   in if null filteredGates
        then Just specialLocBody
        else Just specialLocBody {gatesInfo = filteredGates}
filterGates Nothing _ = Nothing

makeFullSpecialLocation :: Transactionable m => (D.SpecialLocation, Text) -> m SpecialLocationFull
makeFullSpecialLocation (D.SpecialLocation {..}, specialShape) = do
  gatesWithShape <- QGI.findAllGatesBySpecialLocationId id
  let gatesInfoFull =
        map
          ( \(GD.GateInfo {point = gatePoint, id = gateId, gateType = gt, createdAt = _gateCreatedAt, geom = _gateGeom, updatedAt = _gateUpdatedAt, ..}, gateShape) ->
              GD.GateInfoFull
                { GD.id = gateId,
                  GD.point = gatePoint,
                  GD.geoJson = gateShape,
                  GD.gateType = gt,
                  ..
                }
          )
          gatesWithShape
  pure $
    SpecialLocationFull
      { gatesInfo = gatesInfoFull,
        geoJson = Just specialShape,
        ..
      }

fullSpecialLocationRedisKey :: Text
fullSpecialLocationRedisKey = "SpecialLocation:Full"

findFullSpecialLocationsByMerchantOperatingCityId :: (Transactionable m, CacheFlow m r) => Text -> m [SpecialLocationFull]
findFullSpecialLocationsByMerchantOperatingCityId mocId = do
  Hedis.safeGet fullSpecialLocationRedisKey >>= \case
    Just a -> pure a
    Nothing -> do
      specialLocations <- findFullSpecialLocationsByMerchantOperatingCityId' mocId
      Hedis.set fullSpecialLocationRedisKey specialLocations
      pure specialLocations

findFullSpecialLocationsByMerchantOperatingCityId' :: (Transactionable m, CacheFlow m r) => Text -> m [SpecialLocationFull]
findFullSpecialLocationsByMerchantOperatingCityId' mocId = do
  mbRes <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $
        specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
          ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation mbRes

findSpecialLocationByLatLongFull :: Transactionable m => LatLong -> m (Maybe SpecialLocationFull)
findSpecialLocationByLatLongFull point = do
  mbRes <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ containsPoint (point.lon, point.lat)
      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation (listToMaybe mbRes)

findSpecialLocationByLatLongNearby :: Transactionable m => LatLong -> Int -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLongNearby point radius = do
  specialLocations <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ pointCloseByOrWithin (point.lon, point.lat) (val radius)
      return (specialLocation, F.getGeomGeoJSON)
  return $ listToMaybe specialLocations

findPickupSpecialLocationByLatLong :: (Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe D.SpecialLocation)
findPickupSpecialLocationByLatLong point = do
  mbSpecialLocation <- Esq.runInReplica $ findSpecialLocationByLatLong' point
  case mbSpecialLocation of
    Just specialLocation -> pure $ Just specialLocation
    Nothing -> maybe (pure Nothing) (Lib.Queries.SpecialLocation.findById . (.specialLocationId)) =<< QGI.findGateInfoByLatLongWithoutGeoJson point

findSpecialLocationByLatLong' :: Transactionable m => LatLong -> m (Maybe D.SpecialLocation)
findSpecialLocationByLatLong' point = do
  specialLocations <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ containsPoint (point.lon, point.lat)
      return specialLocation
  return $ listToMaybe specialLocations

findSpecialLocationByLatLong :: Transactionable m => LatLong -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLong point = do
  specialLocations <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ containsPoint (point.lon, point.lat)
      return (specialLocation, F.getGeomGeoJSON)
  return $ listToMaybe specialLocations

deleteById :: Id D.SpecialLocation -> SqlDB ()
deleteById = Esq.deleteByKey @SpecialLocationT
