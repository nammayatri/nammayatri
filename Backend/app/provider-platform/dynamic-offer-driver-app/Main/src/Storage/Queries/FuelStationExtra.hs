{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FuelStationExtra where

import Domain.Types.FuelStation
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se

-- | Find a fuel station by ID that is active.
findByIdAndActive :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FuelStation -> m (Maybe FuelStation)
findByIdAndActive stationId = findOneWithKV [Se.And [Se.Is BeamFS.id (Se.Eq (getId stationId)), Se.Is BeamFS.isActive (Se.Eq True)]]

-- | Find nearby fuel stations using application-level Haversine distance calculation.
-- In production with PostGIS, this would use ST_DWithin for geospatial queries.
-- Falls back to fetching all active stations for the operating city and filtering by distance.
findNearbyStations ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Double ->
  Double ->
  Int ->
  Maybe [Text] ->
  Int ->
  m [FuelStation]
findNearbyStations merchantOpCityId lat lon radiusMeters mbFuelTypes limit' = do
  allStations <- findAllWithKV [Se.And [Se.Is BeamFS.merchantOperatingCityId (Se.Eq (getId merchantOpCityId)), Se.Is BeamFS.isActive (Se.Eq True)]]
  let filtered = filterByDistance lat lon radiusMeters $ filterByFuelType mbFuelTypes allStations
  pure $ take limit' $ sortBy (comparing (haversineDistanceM lat lon)) filtered
  where
    filterByFuelType Nothing stations = stations
    filterByFuelType (Just fuelTypes) stations =
      filter (\s -> any (`elem` s.fuelTypes) fuelTypes) stations

    filterByDistance dLat dLon radius stations =
      filter (\s -> haversineDistanceM dLat dLon s <= fromIntegral radius) stations

    haversineDistanceM :: Double -> Double -> FuelStation -> Double
    haversineDistanceM lat1 lon1 station =
      let r = 6371000
          dLat = toRad (station.lat - lat1)
          dLon = toRad (station.lon - lon1)
          a = sin (dLat / 2) ^ (2 :: Int) + cos (toRad lat1) * cos (toRad station.lat) * sin (dLon / 2) ^ (2 :: Int)
          c = 2 * atan2 (sqrt a) (sqrt (1 - a))
       in r * c

    toRad deg = deg * pi / 180
    comparing f x = f x

-- | Find all stations by city with pagination.
findAllByCityPaginated :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> Int -> m [FuelStation]
findAllByCityPaginated city limit' offset' =
  findAllWithOptionsKV
    [Se.Is BeamFS.city (Se.Eq city)]
    (Se.Desc BeamFS.createdAt)
    (Just limit')
    (Just offset')

-- | Find all stations by merchant operating city with pagination.
findAllByMerchantOpCityPaginated :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Int -> Int -> m [FuelStation]
findAllByMerchantOpCityPaginated merchantOpCityId limit' offset' =
  findAllWithOptionsKV
    [Se.Is BeamFS.merchantOperatingCityId (Se.Eq (getId merchantOpCityId))]
    (Se.Desc BeamFS.createdAt)
    (Just limit')
    (Just offset')

-- | Create a new fuel station record.
createFuelStation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FuelStation -> m ()
createFuelStation = createWithKV

-- | Update a fuel station record.
updateFuelStation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FuelStation -> m ()
updateFuelStation station = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamFS.name station.name,
      Se.Set BeamFS.address station.address,
      Se.Set BeamFS.lat station.lat,
      Se.Set BeamFS.lon station.lon,
      Se.Set BeamFS.fuelTypes station.fuelTypes,
      Se.Set BeamFS.brand station.brand,
      Se.Set BeamFS.operatingHoursStart station.operatingHoursStart,
      Se.Set BeamFS.operatingHoursEnd station.operatingHoursEnd,
      Se.Set BeamFS.isOpen24h station.isOpen24h,
      Se.Set BeamFS.phoneNumber station.phoneNumber,
      Se.Set BeamFS.isActive station.isActive,
      Se.Set BeamFS.city station.city,
      Se.Set BeamFS.updatedAt now
    ]
    [Se.Is BeamFS.id (Se.Eq (getId station.id))]

-- | Soft-delete a fuel station by setting isActive to False.
softDeleteFuelStation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id FuelStation -> m ()
softDeleteFuelStation stationId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamFS.isActive False,
      Se.Set BeamFS.updatedAt now
    ]
    [Se.Is BeamFS.id (Se.Eq (getId stationId))]
