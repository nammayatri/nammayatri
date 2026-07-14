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
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.GateInfo.Geometry (minDistanceToPolygonEdges, parseGatePolygons, pointInPolygon)
import qualified Lib.Queries.GateInfo as QGI
import Lib.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Tabular.SpecialLocation
import qualified Lib.Types.GateInfoExtra as GD
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
    createdAt :: UTCTime,
    priority :: Int,
    enabled :: Bool,
    isOpenMarketEnabled :: Bool,
    isQueueEnabled :: Maybe Bool,
    enforceTollRoute :: Maybe Bool,
    supportNumber :: Maybe Text,
    render :: Maybe D.RenderType,
    fareSettlementType :: Maybe D.FareSettlementType
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

findById :: (Transactionable m, EsqDBReplicaFlow m r) => Id D.SpecialLocation -> m (Maybe D.SpecialLocation)
findById = Esq.runInReplica . Esq.findById

findByLocationNameAndCity :: (Transactionable m, EsqDBReplicaFlow m r) => Text -> Text -> m (Maybe D.SpecialLocation)
findByLocationNameAndCity locationName merchanOperatingCityId =
  Esq.runInReplica $
    Esq.findOne $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $
        specialLocation ^. SpecialLocationLocationName ==. val locationName
          &&. specialLocation ^. SpecialLocationMerchantOperatingCityId ==. val (Just merchanOperatingCityId)
      return specialLocation

findByLocationName :: (Transactionable m, EsqDBReplicaFlow m r) => Text -> m (Maybe D.SpecialLocation)
findByLocationName locationName =
  Esq.runInReplica $
    Esq.findOne $ do
      specialLocation <- from $ table @SpecialLocationT
      where_ $
        specialLocation ^. SpecialLocationLocationName ==. val locationName
      return specialLocation

findByIdWithGeom :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => Id D.SpecialLocation -> m (Maybe (D.SpecialLocation, Maybe Text))
findByIdWithGeom specialLocationId = do
  cached <- getAllEnabledSpecialLocationsWithGeom
  pure $ (\(sl, _) -> (sl, sl.geomGeoJson)) <$> find (\(sl, _) -> sl.id == specialLocationId) cached

filterGates :: Maybe SpecialLocationFull -> Bool -> Maybe SpecialLocationFull
filterGates (Just specialLocBody) isOrigin =
  let gateTypeRequired = if isOrigin then [GD.Pickup, GD.Parking] else [GD.Drop]
      filteredGates = filter matchGateType (gatesInfo specialLocBody)
      matchGateType gate = case gate of
        GD.GateInfoFull {GD.gateType = gt} -> gt `elem` gateTypeRequired
   in if null filteredGates
        then Just specialLocBody
        else Just specialLocBody {gatesInfo = filteredGates}
filterGates Nothing _ = Nothing

makeFullSpecialLocation :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => D.SpecialLocation -> m SpecialLocationFull
makeFullSpecialLocation D.SpecialLocation {..} = do
  gatesWithShape <- QGI.findAllGatesBySpecialLocationId id
  let gatesInfoFull =
        map
          ( \(GD.GateInfo {point = gatePoint, id = gateId, gateType = gt, createdAt = _gateCreatedAt, geomGeoJson = _gateGeom, updatedAt = _gateUpdatedAt, merchantId = _merchantId, merchantOperatingCityId = _merchantOperatingCityId, ..}, gateShape) ->
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
        geoJson = geomGeoJson,
        merchantOperatingCityId = getId <$> merchantOperatingCityId,
        ..
      }

buildSpecialLocationWithGates :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => D.SpecialLocation -> m D.SpecialLocation
buildSpecialLocationWithGates specialLocation = do
  gates <- QGI.findAllGatesBySpecialLocationIdWithoutGeoJson specialLocation.id
  pure specialLocation {D.gates = map (\(GD.GateInfo {..}) -> D.GatesInfo {..}) gates}

-- | Drop every special-zone L1 in-mem cache entry — full special locations, the
--   all-enabled-with-geom set used for lat/long lookups, warrior lists, and the gate
--   caches — and propagate the cleanup to other pods via the InMem sidecar. Call this
--   after any special-location or gate mutation so reads aren't served stale until TTL.
clearSpecialZoneInMemCache :: (MonadFlow m, CacheFlow m r) => m ()
clearSpecialZoneInMemCache = do
  IM.refreshInMem "SpecialLocation:"
  IM.refreshInMem "GateInfo:"
  IM.refreshInMem "GateProximityPolygons"

fullSpecialLocationRedisKey :: Text -> Text
fullSpecialLocationRedisKey city = "SpecialLocation:Full:" <> city

specialLocationWarriorRedisKey :: Text -> Text -> Text
specialLocationWarriorRedisKey category mocId = "SpecialLocation:Warrior:" <> category <> ":" <> mocId

findFullSpecialLocationsByMerchantOperatingCityId :: (BeamFlow m r, Transactionable m, CacheFlow m r, EsqDBReplicaFlow m r, MonadFlow m) => Text -> m [SpecialLocationFull]
findFullSpecialLocationsByMerchantOperatingCityId mocId = do
  let cacheKey = fullSpecialLocationRedisKey mocId
  IM.withInMemCache [cacheKey] 3600 $
    Hedis.safeGet cacheKey >>= \case
      Just a -> pure a
      Nothing -> do
        specialLocations <- findFullSpecialLocationsByMerchantOperatingCityId' mocId
        Hedis.set cacheKey specialLocations
        pure specialLocations

findFullSpecialLocationsByMerchantOperatingCityId' :: (BeamFlow m r, Transactionable m, CacheFlow m r, EsqDBReplicaFlow m r) => Text -> m [SpecialLocationFull]
findFullSpecialLocationsByMerchantOperatingCityId' mocId = do
  mbRes <-
    Esq.runInReplica $
      Esq.findAll $ do
        specialLocation <- from $ table @SpecialLocationT
        where_ $
          specialLocation ^. SpecialLocationEnabled ==. val True
            &&. ( specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
                    ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
                )
        return specialLocation
  mapM makeFullSpecialLocation mbRes

findAllSpecialLocationsWithGeoJSON ::
  (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe [D.SpecialLocationType] ->
  m [SpecialLocationFull]
findAllSpecialLocationsWithGeoJSON mocId mbLimit mbOffset mbLocationTypes = do
  mbRes <-
    Esq.runInReplica $
      Esq.findAll $ do
        specialLocation <- from $ table @SpecialLocationT
        where_ $
          ( specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
              ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
          )
            &&. case mbLocationTypes of
              Just locationTypes -> specialLocation ^. SpecialLocationLocationType `in_` valList (map (Just) locationTypes)
              Nothing -> val True
        orderBy [asc (specialLocation ^. SpecialLocationPriority)]

        forM_ mbLimit (limit . fromIntegral)
        forM_ mbOffset (offset . fromIntegral)

        return specialLocation
  mapM makeFullSpecialLocation mbRes

findAllSpecialLocationsWithGeoJSONAllCities ::
  (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Maybe [D.SpecialLocationType] ->
  m [SpecialLocationFull]
findAllSpecialLocationsWithGeoJSONAllCities mbLimit mbOffset mbLocationTypes = do
  mbRes <-
    Esq.runInReplica $
      Esq.findAll $ do
        specialLocation <- from $ table @SpecialLocationT
        where_ $
          case mbLocationTypes of
            Just locationTypes -> specialLocation ^. SpecialLocationLocationType `in_` valList (map (Just) locationTypes)
            Nothing -> val True
        orderBy [asc (specialLocation ^. SpecialLocationPriority)]

        forM_ mbLimit (limit . fromIntegral)
        forM_ mbOffset (offset . fromIntegral)

        return specialLocation
  mapM makeFullSpecialLocation mbRes

findSpecialLocationsWarriorByMerchantOperatingCityId :: (BeamFlow m r, Transactionable m, CacheFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> m [D.SpecialLocation]
findSpecialLocationsWarriorByMerchantOperatingCityId mocId category = do
  Hedis.safeGet (specialLocationWarriorRedisKey category mocId) >>= \case
    Just a -> pure a
    Nothing -> do
      specialLocations <- findSpecialLocationsWarriorByMerchantOperatingCityId' mocId category
      Hedis.set (specialLocationWarriorRedisKey category mocId) specialLocations
      pure specialLocations

findSpecialLocationsWarriorByMerchantOperatingCityId' :: (BeamFlow m r, Transactionable m, CacheFlow m r, EsqDBReplicaFlow m r) => Text -> Text -> m [D.SpecialLocation]
findSpecialLocationsWarriorByMerchantOperatingCityId' mocId category = do
  specialLocationWithoutGates <-
    Esq.runInReplica $
      Esq.findAll $ do
        specialLocation <- from $ table @SpecialLocationT
        where_ $
          specialLocation ^. SpecialLocationEnabled ==. val True
            &&. ( specialLocation ^. SpecialLocationMerchantOperatingCityId ==. just (val mocId)
                    ||. isNothing (specialLocation ^. SpecialLocationMerchantOperatingCityId)
                    &&. specialLocation ^. SpecialLocationCategory ==. val category
                )
        return specialLocation
  mapM buildSpecialLocationWithGates specialLocationWithoutGates

-- | All enabled special locations with their zone polygon(s) parsed from the
--   @geom_geo_json@ column, held in an in-memory cache (1h TTL) and ordered by
--   priority. Replaces the per-call PostGIS @ST_Contains@/@ST_DWithin@ scans with an
--   in-Haskell point-in-polygon over this cached set, so the read path needs no
--   PostGIS. NOTE: a stale read is possible for up to the TTL after an admin edit.
getAllEnabledSpecialLocationsWithGeom :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => m [(D.SpecialLocation, [[[LatLong]]])]
getAllEnabledSpecialLocationsWithGeom =
  IM.withInMemCache ["SpecialLocation:AllEnabledWithGeom"] 3600 $ do
    sls <-
      Esq.runInReplica $
        Esq.findAll $ do
          specialLocation <- from $ table @SpecialLocationT
          where_ $ specialLocation ^. SpecialLocationEnabled ==. val True
          orderBy [asc (specialLocation ^. SpecialLocationPriority)]
          return specialLocation
    pure $ map (\sl -> (sl, fromMaybe [] (sl.geomGeoJson >>= parseGatePolygons))) sls

findSpecialLocationByLatLongFull :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe SpecialLocationFull)
findSpecialLocationByLatLongFull point = do
  cached <- getAllEnabledSpecialLocationsWithGeom
  case listToMaybe [sl | (sl, polys) <- cached, any (pointInPolygon point) polys] of
    Just sl -> Just <$> makeFullSpecialLocation sl
    Nothing -> pure Nothing

findSpecialLocationByLatLongNearby :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> Int -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLongNearby point radius = do
  cached <- getAllEnabledSpecialLocationsWithGeom
  let isNear polys =
        any (pointInPolygon point) polys
          || any (\poly -> minDistanceToPolygonEdges point poly < fromIntegral radius) polys
  pure $ listToMaybe [(sl, fromMaybe "" sl.geomGeoJson) | (sl, polys) <- cached, isNear polys]

findPickupSpecialLocationByLatLong :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe D.SpecialLocation)
findPickupSpecialLocationByLatLong point = do
  mbSpecialLocation <- findSpecialLocationByLatLong' point
  case mbSpecialLocation of
    Just specialLocation -> pure $ Just specialLocation
    Nothing -> maybe (pure Nothing) (Lib.Queries.SpecialLocation.findById . (.specialLocationId)) =<< QGI.findGateInfoByLatLongWithoutGeoJson point

findSpecialLocationByLatLong' :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe D.SpecialLocation)
findSpecialLocationByLatLong' point = do
  cached <- getAllEnabledSpecialLocationsWithGeom
  pure $ listToMaybe [sl | (sl, polys) <- cached, any (pointInPolygon point) polys]

findSpecialLocationByLatLong :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe (D.SpecialLocation, Text))
findSpecialLocationByLatLong point = do
  cached <- getAllEnabledSpecialLocationsWithGeom
  pure $ listToMaybe [(sl, fromMaybe "" sl.geomGeoJson) | (sl, polys) <- cached, any (pointInPolygon point) polys]

deleteById :: Id D.SpecialLocation -> SqlDB ()
deleteById = Esq.deleteByKey @SpecialLocationT

specialLocToSpecialLocWarrior :: (Transactionable m, EsqDBReplicaFlow m r) => D.SpecialLocation -> m SpecialLocationWarrior
specialLocToSpecialLocWarrior D.SpecialLocation {..} = do
  linkedLocations <- mapM Lib.Queries.SpecialLocation.findById linkedLocationsIds >>= pure . catMaybes
  pure SpecialLocationWarrior {merchantOperatingCityId = getId <$> merchantOperatingCityId, ..}

specialLocFullToSpecialLocWarrior :: (Transactionable m, EsqDBReplicaFlow m r) => SpecialLocationFull -> m SpecialLocationWarrior
specialLocFullToSpecialLocWarrior SpecialLocationFull {..} = do
  linkedLocations <- mapM Lib.Queries.SpecialLocation.findById linkedLocationsIds >>= pure . catMaybes
  pure SpecialLocationWarrior {..}
