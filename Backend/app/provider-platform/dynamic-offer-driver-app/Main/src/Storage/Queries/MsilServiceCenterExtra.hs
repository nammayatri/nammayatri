module Storage.Queries.MsilServiceCenterExtra
  ( findNearbyActiveCenters,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MsilServiceCenter as DMSC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.MsilServiceCenter as BeamMSC

-- | Find nearby active service centers using Haversine approximation.
-- In production, this would use PostGIS ST_DWithin for efficiency.
-- Falls back to application-level distance filtering for portability.
findNearbyActiveCenters ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Double -> -- lat
  Double -> -- lon
  Int -> -- radius in meters
  Maybe Text -> -- center type filter
  Int -> -- limit
  m [(DMSC.MsilServiceCenter, Maybe Int)]
findNearbyActiveCenters merchantOpCityId lat lon radius mbCenterType limit = do
  -- Fetch all active centers for this operating city
  -- In production, use raw SQL with PostGIS: ST_DWithin(point, ST_SetSRID(ST_MakePoint(lon, lat), 4326), radius)
  allCenters <- findAllWithKV
    [ Se.And
        [ Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq (getId merchantOpCityId),
          Se.Is BeamMSC.isActive $ Se.Eq True
        ]
    ]
  let filtered = case mbCenterType of
        Just ct -> filter (\c -> c.centerType == ct) allCenters
        Nothing -> allCenters
      -- Calculate Haversine distance and filter within radius
      withDistance = mapMaybe (\c ->
        let dist = haversineDistance lat lon c.lat c.lon
        in if dist <= fromIntegral radius
           then Just (c, Just (round dist :: Int))
           else Nothing
        ) filtered
      -- Sort by distance and limit
      sorted = take limit $ sortOn snd withDistance
  return sorted

-- | Haversine formula for distance in meters between two lat/lon points
haversineDistance :: Double -> Double -> Double -> Double -> Double
haversineDistance lat1 lon1 lat2 lon2 =
  let r = 6371000 -- Earth radius in meters
      dLat = toRadians (lat2 - lat1)
      dLon = toRadians (lon2 - lon1)
      a = sin (dLat / 2) ^ (2 :: Int) + cos (toRadians lat1) * cos (toRadians lat2) * sin (dLon / 2) ^ (2 :: Int)
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in r * c

toRadians :: Double -> Double
toRadians deg = deg * pi / 180
