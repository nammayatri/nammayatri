{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Queries.SpecialLocation where

import Control.Applicative ((<|>))
import qualified Crypto.Hash as Hash
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.List as DL
import qualified Data.Text as T
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.GateInfo.Geometry (minDistanceToPolygonEdges, parseGatePolygons, pointInPolygon, polygonsOverlap)
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocationPriority as QSLP
import Lib.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Tabular.SpecialLocation
import qualified Lib.Types.GateInfoExtra as GD
import qualified Lib.Types.SpecialLocation as D
import qualified Lib.Types.SpecialLocationPriority as Fields

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
    -- | Queue polygon, if configured separately from the pricing polygon ('geoJson').
    queueGeoJson :: Maybe Text,
    createdAt :: UTCTime,
    priority :: Int,
    enabled :: Bool,
    isOpenMarketEnabled :: Bool,
    isQueueEnabled :: Maybe Bool,
    parkingFeeExemptionEnabled :: Maybe Bool,
    enforceTollRoute :: Maybe Bool,
    fetchAllGateFareProduct :: Maybe Bool,
    supportNumber :: Maybe Text,
    render :: Maybe D.RenderType,
    paymentModes :: Maybe [D.PaymentMode],
    fareSettlementType :: Maybe D.FareSettlementType,
    poolingMode :: Maybe D.PoolingMode,
    pickupPriority :: Maybe Int,
    dropPriority :: Maybe Int
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
        queueGeoJson = queueGeomGeoJson,
        merchantOperatingCityId = getId <$> merchantOperatingCityId,
        pickupPriority = Nothing,
        dropPriority = Nothing,
        ..
      }

enrichSpecialLocationsWithPriority ::
  (Transactionable m, EsqDBReplicaFlow m r) => Text -> [SpecialLocationFull] -> m [SpecialLocationFull]
enrichSpecialLocationsWithPriority merchantId sls = do
  priorities <- QSLP.findAllByMerchantId merchantId
  pure $ map (enrich priorities) sls
  where
    enrich priorities sl =
      let rows = filter (\p -> p.category == sl.category) priorities
          mocId = fromMaybe "" sl.merchantOperatingCityId
          mbPriority = case rows of
            [single] | T.null single.merchantOperatingCityId -> Just single
            _ -> find (\p -> p.merchantOperatingCityId == mocId) rows
       in sl
            { pickupPriority = (.pickupPriority) <$> mbPriority,
              dropPriority = (.dropPriority) <$> mbPriority
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

-- | Drop the Redis copy of the driver-app special-location list ('findFullSpecialLocationsByMerchantOperatingCityId')
--   and its ETag for the given operating cities, so the next request rebuilds them from the DB.
--   Both keys must go together: a surviving ETag would keep answering 304 for the old list.
--   Zones with no operating city appear in every city's list; those are only refreshed by the
--   key TTL ('fullSpecialLocationCacheTtl').
--   Call this before 'clearSpecialZoneInMemCache': the in-memory list is loaded from this Redis
--   key, so refreshing memory first lets a request in between reload the old list for up to 1h.
clearSpecialLocationListCache :: (CacheFlow m r) => [Text] -> m ()
clearSpecialLocationListCache mocIds =
  forM_ (DL.nub mocIds) $ \mocId -> do
    Hedis.del (fullSpecialLocationRedisKey mocId)
    Hedis.del (fullSpecialLocationHashRedisKey mocId)

-- | Backstop expiry for the driver-app list and ETag keys, matching the 1-hour in-mem TTL.
fullSpecialLocationCacheTtl :: Hedis.ExpirationTime
fullSpecialLocationCacheTtl = 3600

fullSpecialLocationRedisKey :: Text -> Text
fullSpecialLocationRedisKey city = "SpecialLocation:Full:" <> city

fullSpecialLocationHashRedisKey :: Text -> Text
fullSpecialLocationHashRedisKey city = "SpecialLocation:Full:Hash:" <> city

computeSpecialLocationListETag :: [SpecialLocationFull] -> Text
computeSpecialLocationListETag locs =
  T.cons '"' (T.pack (show (Hash.hash @Hash.SHA256 (BS.toStrict (encode locs))))) `T.snoc` '"'

getFullSpecialLocationHash :: (CacheFlow m r) => Text -> m (Maybe Text)
getFullSpecialLocationHash mocId = Hedis.safeGet (fullSpecialLocationHashRedisKey mocId)

setFullSpecialLocationHash :: (CacheFlow m r) => Text -> Text -> m ()
setFullSpecialLocationHash mocId eTag = Hedis.setExp (fullSpecialLocationHashRedisKey mocId) eTag fullSpecialLocationCacheTtl

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
        Hedis.setExp cacheKey specialLocations fullSpecialLocationCacheTtl
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

-- | Same set as 'getAllEnabledSpecialLocationsWithGeom', but each zone carries its queue
--   polygon ('D.queueGeomGeoJson'), falling back to the pricing polygon when none is set.
--   Use this for queue-facing lookups only; pricing and gates must keep using the pricing polygon.
getAllEnabledSpecialLocationsWithQueueGeom :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => m [(D.SpecialLocation, [[[LatLong]]])]
getAllEnabledSpecialLocationsWithQueueGeom =
  IM.withInMemCache ["SpecialLocation:AllEnabledWithQueueGeom"] 3600 $
    map (\(sl, pricingPolys) -> (sl, fromMaybe pricingPolys (sl.queueGeomGeoJson >>= parseGatePolygons))) <$> getAllEnabledSpecialLocationsWithGeom

-- | Serve the queue polygon as 'geoJson' (falls back to the pricing polygon when none is set).
--   For consumers whose notion of "inside the zone" is about the driver queue: LTS and the driver app.
--   'queueGeoJson' is dropped so the (large) polygon isn't sent twice; neither consumer reads it.
withQueueGeoJson :: SpecialLocationFull -> SpecialLocationFull
withQueueGeoJson sl = sl {geoJson = sl.queueGeoJson <|> sl.geoJson, queueGeoJson = Nothing}

-- | First zone (by priority) whose queue polygon contains the point.
findInQueueBand :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe D.SpecialLocation)
findInQueueBand point = do
  cached <- getAllEnabledSpecialLocationsWithQueueGeom
  pure $ listToMaybe [sl | (sl, polys) <- cached, any (pointInPolygon point) polys]

-- | Zone the driver is queueing for. A pricing-polygon match wins, so a driver standing inside a
--   smaller zone that lies within another zone's wider queue band resolves to the smaller zone.
--   Only when no pricing polygon contains the point is the queue band consulted.
findQueueSpecialLocationByLatLongFull :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe SpecialLocationFull)
findQueueSpecialLocationByLatLongFull point =
  findSpecialLocationByLatLong' point
    >>= maybe (findInQueueBand point) (pure . Just)
    >>= mapM makeFullSpecialLocation

-- | Queue-facing variant of 'findPickupSpecialLocationByLatLong': pricing polygon, then queue
--   band, then the existing gate fallback.
findQueueSpecialLocationByLatLong :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => LatLong -> m (Maybe D.SpecialLocation)
findQueueSpecialLocationByLatLong point =
  findSpecialLocationByLatLong' point
    >>= maybe (findInQueueBand point) (pure . Just)
    >>= maybe (findPickupSpecialLocationByLatLong point) (pure . Just)

-- | Enabled zones (other than 'selfId') that the queue band newly covers: their pricing polygon
--   overlaps the queue polygon but not the zone's own pricing polygon. Zones already overlapping
--   the own pricing polygon (e.g. a parking lot nested inside an airport) are an existing setup the
--   band doesn't change, so they are not reported. The driver app and LTS pick a zone by their own
--   rules when polygons overlap, so a queue band must not reach into another zone.
findZonesOverlappingQueueGeom :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => Maybe (Id D.SpecialLocation) -> Maybe Text -> Text -> m [D.SpecialLocation]
findZonesOverlappingQueueGeom selfId mbOwnPricingGeoJson queueGeoJson = do
  let queuePolys = fromMaybe [] (parseGatePolygons queueGeoJson)
      ownPolys = fromMaybe [] (mbOwnPricingGeoJson >>= parseGatePolygons)
  cached <- getAllEnabledSpecialLocationsWithGeom
  pure
    [ sl
      | (sl, pricingPolys) <- cached,
        Just sl.id /= selfId,
        overlapsAny queuePolys pricingPolys,
        not (overlapsAny ownPolys pricingPolys)
    ]

-- | Reject a queue polygon that reaches into another enabled zone (see 'findZonesOverlappingQueueGeom').
validateQueueGeomNoOverlap :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => Maybe (Id D.SpecialLocation) -> Text -> Maybe Text -> Text -> m ()
validateQueueGeomNoOverlap selfId locationName mbOwnPricingGeoJson queueGeoJson = do
  case parseGatePolygons queueGeoJson of
    Nothing -> throwError $ InvalidRequest $ "Queue polygon for " <> locationName <> " is not a valid Polygon/MultiPolygon"
    Just _ -> pure ()
  overlapping <- findZonesOverlappingQueueGeom selfId mbOwnPricingGeoJson queueGeoJson
  unless (null overlapping) $
    throwError $
      InvalidRequest $
        "Queue polygon for " <> locationName <> " overlaps the area of: "
          <> T.intercalate ", " (map (\sl -> sl.locationName <> " (" <> sl.id.getId <> ")") overlapping)
          <> ". A queue band must not cover another zone."

-- | Reverse of 'findZonesOverlappingQueueGeom': enabled zones (other than 'selfId') whose queue
--   band the given pricing polygon reaches into, excluding zones whose own pricing polygon it
--   already overlaps (a zone nested inside an airport is an existing setup, not a band issue).
findQueueBandsOverlappingPricingGeom :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => Maybe (Id D.SpecialLocation) -> Text -> m [D.SpecialLocation]
findQueueBandsOverlappingPricingGeom selfId pricingGeoJson = do
  let pricingPolys = fromMaybe [] (parseGatePolygons pricingGeoJson)
  cached <- getAllEnabledSpecialLocationsWithGeom
  pure
    [ sl
      | (sl, otherPricingPolys) <- cached,
        Just sl.id /= selfId,
        Just bandPolys <- [sl.queueGeomGeoJson >>= parseGatePolygons],
        overlapsAny pricingPolys bandPolys,
        not (overlapsAny pricingPolys otherPricingPolys)
    ]

-- | Reject a pricing polygon that lies in another zone's queue band, so a zone added after a band
--   was set can't end up inside it. A polygon that doesn't parse is left to the upload's own checks.
validatePricingGeomOutsideQueueBands :: (BeamFlow m r, Transactionable m, EsqDBReplicaFlow m r) => Maybe (Id D.SpecialLocation) -> Text -> Text -> m ()
validatePricingGeomOutsideQueueBands selfId locationName pricingGeoJson = do
  overlapping <- findQueueBandsOverlappingPricingGeom selfId pricingGeoJson
  unless (null overlapping) $
    throwError $
      InvalidRequest $
        "Area of " <> locationName <> " lies inside the queue band of: "
          <> T.intercalate ", " (map (\sl -> sl.locationName <> " (" <> sl.id.getId <> ")") overlapping)
          <> ". Shrink that queue band first."

overlapsAny :: [[[LatLong]]] -> [[[LatLong]]] -> Bool
overlapsAny as bs = or [polygonsOverlap a b | a <- as, b <- bs]

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

-- | Single-column update. Deliberately not routed through
--   'Lib.Queries.SpecialLocationGeom.updateSpecialLocation', which writes the whole row and
--   would blank the geometry when handed a domain value read back without it.
updateParkingFeeExemptionEnabled :: Maybe Bool -> UTCTime -> Id D.SpecialLocation -> SqlDB ()
updateParkingFeeExemptionEnabled parkingFeeExemptionEnabled now specialLocationId = Esq.update $ \tbl -> do
  set
    tbl
    [ SpecialLocationParkingFeeExemptionEnabled =. val parkingFeeExemptionEnabled,
      SpecialLocationUpdatedAt =. val now
    ]
  where_ $ tbl ^. SpecialLocationUuid ==. val specialLocationId.getId

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
