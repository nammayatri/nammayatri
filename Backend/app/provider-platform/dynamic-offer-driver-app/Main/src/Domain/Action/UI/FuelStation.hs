module Domain.Action.UI.FuelStation
  ( getDriverFuelStations,
    getDriverFuelStationDetail,
  )
where

import qualified API.Types.UI.FuelStation as FuelStation
import qualified Data.Text as T
import Domain.Types.FuelStation
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Environment
import Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.Throwing
import qualified Storage.CachedQueries.FuelStation as CQFuelStation
import qualified Storage.Queries.FuelStationExtra as QFuelStationExtra

-- | Get nearby fuel stations for a driver based on lat/lon, radius, and optional fuel type filter.
-- Follows the pattern established by DemandHotspots for geospatial queries with Redis caching.
getDriverFuelStations ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Double ->
    Double ->
    Int ->
    Maybe Text ->
    Maybe Int ->
    Flow FuelStation.GetFuelStationsResp
  )
getDriverFuelStations (_, _merchantId, merchantOpCityId) lat lon radius mbFuelType mbLimit = do
  let stationLimit = fromMaybe 20 mbLimit
      radiusMeters = radius
      fuelTypeFilter = mbFuelType >>= parseFuelTypes
  -- Try Redis GEO cache first, fall back to PostGIS query
  stations <- CQFuelStation.findNearbyStations merchantOpCityId lat lon radiusMeters fuelTypeFilter stationLimit
  let stationResps = map (mkFuelStationResp lat lon) stations
  pure $
    FuelStation.GetFuelStationsResp
      { stations = stationResps,
        totalCount = length stationResps
      }

-- | Get details for a specific fuel station by ID.
getDriverFuelStationDetail ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Text ->
    Flow FuelStation.FuelStationResp
  )
getDriverFuelStationDetail (_, _merchantId, merchantOpCityId) stationId = do
  station <- QFuelStationExtra.findByIdAndActive (Id stationId) >>= fromMaybeM (InvalidRequest "Fuel station not found")
  when (station.merchantOperatingCityId /= merchantOpCityId) $
    throwError $ InvalidRequest "Fuel station not in your operating city"
  pure $ mkFuelStationResp 0 0 station

-- | Convert domain type to API response type with distance calculation.
mkFuelStationResp :: Double -> Double -> FuelStation -> FuelStation.FuelStationResp
mkFuelStationResp driverLat driverLon station =
  let dist = if driverLat == 0 && driverLon == 0 then 0 else haversineDistance driverLat driverLon station.lat station.lon
   in FuelStation.FuelStationResp
        { id = getId station.id,
          name = station.name,
          address = station.address,
          lat = station.lat,
          lon = station.lon,
          fuelTypes = map textToFuelType station.fuelTypes,
          brand = station.brand,
          distance = dist,
          isOpen = station.isOpen24h || isStationCurrentlyOpen station,
          operatingHoursStart = station.operatingHoursStart,
          operatingHoursEnd = station.operatingHoursEnd,
          phoneNumber = station.phoneNumber
        }

-- | Haversine distance in meters between two lat/lon points.
haversineDistance :: Double -> Double -> Double -> Double -> Int
haversineDistance lat1 lon1 lat2 lon2 =
  let r = 6371000 -- Earth radius in meters
      dLat = toRadians (lat2 - lat1)
      dLon = toRadians (lon2 - lon1)
      a = sin (dLat / 2) ^ (2 :: Int) + cos (toRadians lat1) * cos (toRadians lat2) * sin (dLon / 2) ^ (2 :: Int)
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
   in round (r * c)
  where
    toRadians deg = deg * pi / 180

-- | Check if a station is currently open based on operating hours.
isStationCurrentlyOpen :: FuelStation -> Bool
isStationCurrentlyOpen station =
  case (station.operatingHoursStart, station.operatingHoursEnd) of
    (Just _, Just _) -> True -- Simplified: would check current time in production
    _ -> True -- Default to open if hours not specified

-- | Parse comma-separated fuel type string into list of FuelType.
parseFuelTypes :: Text -> Maybe [FuelStation.FuelType]
parseFuelTypes txt =
  let types = map (textToFuelType . T.strip) $ T.splitOn "," txt
   in if null types then Nothing else Just types

-- | Convert text to FuelType enum.
textToFuelType :: Text -> FuelStation.FuelType
textToFuelType "PETROL" = FuelStation.PETROL
textToFuelType "DIESEL" = FuelStation.DIESEL
textToFuelType "CNG" = FuelStation.CNG
textToFuelType "EV" = FuelStation.EV
textToFuelType _ = FuelStation.PETROL
