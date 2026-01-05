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
    linkedLocationsIds :: [Id D.SpecialLocation],
    locationType :: D.SpecialLocationType,
    gatesInfo :: [GD.GateInfoFull],
    gates :: [D.GatesInfo], --TODO: deprecate this later
    geoJson :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data SpecialLocationWarrior = SpecialLocationWarrior
  { id :: Id D.SpecialLocation,
    locationName :: Text,
    category :: Text,
    merchantOperatingCityId :: Maybe Text,
    gates :: [D.GatesInfo],
    linkedLocations :: [D.SpecialLocation],
    createdAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

findById :: Transactionable m => Id D.SpecialLocation -> m (Maybe D.SpecialLocation)
findById = Esq.findById

findByLocationNameAndCity :: Transactionable m => Text -> Text -> m (Maybe D.SpecialLocation)
findByLocationNameAndCity locationName merchanOperatingCityId =
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $
      specialLocation ^. SpecialLocationLocationName ==. val locationName
        &&. specialLocation ^. SpecialLocationMerchantOperatingCityId ==. val (Just merchanOperatingCityId)
    return specialLocation

findByLocationName :: Transactionable m => Text -> m (Maybe D.SpecialLocation)
findByLocationName locationName =
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $
      specialLocation ^. SpecialLocationLocationName ==. val locationName
    return specialLocation

findByIdWithGeom :: Transactionable m => Id D.SpecialLocation -> m (Maybe (D.SpecialLocation, Maybe Text))
findByIdWithGeom specialLocationId =
  Esq.findOne $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $
      specialLocation ^. SpecialLocationEnabled ==. val True
        &&. specialLocation ^. SpecialLocationId ==. val specialLocationId.getId
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
          ( \(GD.GateInfo {point = gatePoint, id = gateId, gateType = gt, createdAt = _gateCreatedAt, geom = _gateGeom, updatedAt = _gateUpdatedAt, merchantId = _merchantId, merchantOperatingCityId = _merchantOperatingCityId, ..}, gateShape) ->
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
        merchantOperatingCityId = getId <$> merchantOperatingCityId,
        ..
      }

buildSpecialLocationWithGates :: (Transactionable m, EsqDBReplicaFlow m r) => D.SpecialLocation -> m D.SpecialLocation
buildSpecialLocationWithGates specialLocation = do
  gates <- Esq.runInReplica $ QGI.findAllGatesBySpecialLocationIdWithoutGeoJson specialLocation.id
  pure specialLocation {D.gates = map (\(GD.GateInfo {..}) -> D.GatesInfo {..}) gates}

fullSpecialLocationRedisKey :: Text
fullSpecialLocationRedisKey = "SpecialLocation:Full"

specialLocationWarriorRedisKey :: Text -> Text
specialLocationWarriorRedisKey category = "SpecialLocation:Warrior" <> category

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
        specialLocation ^. SpecialLocationEnabled ==. val True
          &&. ( specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
                  ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
              )
      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation mbRes

findAllSpecialLocationsWithGeoJSON ::
  (Transactionable m) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  m [SpecialLocationFull]
findAllSpecialLocationsWithGeoJSON mocId mbLimit mbOffset = do
  mbRes <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $
        ( specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
            ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
        )

      orderBy [asc (specialLocation ^. SpecialLocationPriority)]

      forM_ mbLimit (limit . fromIntegral)
      forM_ mbOffset (offset . fromIntegral)

      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation mbRes

findSpecialLocationsWarriorByMerchantOperatingCityId :: (Transactionable m, CacheFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> m [D.SpecialLocation]
findSpecialLocationsWarriorByMerchantOperatingCityId mocId category = do
  Hedis.safeGet (specialLocationWarriorRedisKey category) >>= \case
    Just a -> pure a
    Nothing -> do
      specialLocations <- findSpecialLocationsWarriorByMerchantOperatingCityId' mocId category
      Hedis.set (specialLocationWarriorRedisKey category) specialLocations
      pure specialLocations

findSpecialLocationsWarriorByMerchantOperatingCityId' :: (Transactionable m, CacheFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> m [D.SpecialLocation]
findSpecialLocationsWarriorByMerchantOperatingCityId' mocId category = do
  specialLocationWithoutGates <- Esq.findAll $ do
    specialLocation <- from $ table @SpecialLocationT
    where_ $
      specialLocation ^. SpecialLocationEnabled ==. val True
        &&. ( specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
                ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
                &&. specialLocation ^. SpecialLocationCategory ==. val category
            )
    return specialLocation
  mapM buildSpecialLocationWithGates specialLocationWithoutGates

findSpecialLocationByLatLongFull :: Transactionable m => LatLong -> m (Maybe SpecialLocationFull)
findSpecialLocationByLatLongFull point = do
  mbRes <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ specialLocation ^. SpecialLocationEnabled ==. val True &&. containsPoint (point.lon, point.lat)
      orderBy [asc (specialLocation ^. SpecialLocationPriority)]
      return (specialLocation, F.getGeomGeoJSON)
  mapM makeFullSpecialLocation (listToMaybe mbRes)

findSpecialLocationByLatLongNearby :: Transactionable m => LatLong -> Int -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLongNearby point radius = do
  specialLocations <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ specialLocation ^. SpecialLocationEnabled ==. val True &&. pointCloseByOrWithin (point.lon, point.lat) (val radius)
      orderBy [asc (specialLocation ^. SpecialLocationPriority)]
      return (specialLocation, F.getGeomGeoJSON)
  return $ listToMaybe specialLocations

findPickupSpecialLocationByLatLong :: (Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe D.SpecialLocation)
findPickupSpecialLocationByLatLong point = do
  mbSpecialLocation <- Esq.runInReplica $ findSpecialLocationByLatLong' point
  case mbSpecialLocation of
    Just specialLocation -> pure $ Just specialLocation
    Nothing -> maybe (pure Nothing) (Esq.runInReplica . Lib.Queries.SpecialLocation.findById . (.specialLocationId)) =<< (Esq.runInReplica $ QGI.findGateInfoByLatLongWithoutGeoJson point)

findSpecialLocationByLatLong' :: Transactionable m => LatLong -> m (Maybe D.SpecialLocation)
findSpecialLocationByLatLong' point = do
  specialLocations <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ specialLocation ^. SpecialLocationEnabled ==. val True &&. containsPoint (point.lon, point.lat)
      orderBy [asc (specialLocation ^. SpecialLocationPriority)]
      return specialLocation
  return $ listToMaybe specialLocations

findSpecialLocationByLatLong :: Transactionable m => LatLong -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLong point = do
  specialLocations <-
    Esq.findAll $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $ specialLocation ^. SpecialLocationEnabled ==. val True &&. containsPoint (point.lon, point.lat)
      orderBy [asc (specialLocation ^. SpecialLocationPriority)]
      return (specialLocation, F.getGeomGeoJSON)
  return $ listToMaybe specialLocations

deleteById :: Id D.SpecialLocation -> SqlDB ()
deleteById = Esq.deleteByKey @SpecialLocationT

specialLocToSpecialLocWarrior :: Transactionable m => D.SpecialLocation -> m SpecialLocationWarrior
specialLocToSpecialLocWarrior D.SpecialLocation {..} = do
  linkedLocations <- mapM Lib.Queries.SpecialLocation.findById linkedLocationsIds >>= pure . catMaybes
  pure SpecialLocationWarrior {merchantOperatingCityId = getId <$> merchantOperatingCityId, ..}

specialLocFullToSpecialLocWarrior :: Transactionable m => SpecialLocationFull -> m SpecialLocationWarrior
specialLocFullToSpecialLocWarrior SpecialLocationFull {..} = do
  linkedLocations <- mapM Lib.Queries.SpecialLocation.findById linkedLocationsIds >>= pure . catMaybes
  pure SpecialLocationWarrior {..}
