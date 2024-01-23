{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.HotSpot where

import Control.Monad.Extra (mapMaybeM)
import qualified Data.Geohash as DG
import qualified Data.List as Dl
import Data.Ord
import qualified Data.Text as Dt
import Domain.Types.HotSpot as HotSpot
import Domain.Types.HotSpotConfig
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess ()
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import Storage.CachedQueries.Maps.LocationMapCache
import Tools.Auth
import qualified Tools.Maps as Maps

type API =
  GetHotSpotAPI

type GetHotSpotAPI = "getHotSpot" :> TokenAuth :> ReqBody '[JSON] Maps.LatLong :> Get '[JSON] HotSpotResponse

handler :: FlowServer API
handler =
  getHotSpot

getHotSpot :: (Id Person.Person, Id Merchant.Merchant) -> Maps.LatLong -> FlowHandler HotSpotResponse
getHotSpot (_, merchantId) latlong = withFlowHandlerAPI (getHotspot latlong merchantId)

allPreciseHotSpot :: Double -> Int -> Maps.LatLong -> [String] -> [String]
allPreciseHotSpot hotSpotRadius maxLen currentCoordinate allPossibleGeoHashes =
  if maxLen > 0
    then
      let possibleGeoHashes = [geohash ++ [suffix] | geohash <- allPossibleGeoHashes, suffix <- "0123456789bcdefghjkmnpqrstuvwxyz"]
          possibleGeoHashesWithInRadius =
            filter
              ( \geohash ->
                  let geoHashLatLong :: Maybe (Double, Double) = DG.decode geohash
                   in case geoHashLatLong of
                        Just (lat, lon) -> fromIntegral (highPrecMetersToMeters (distanceBetweenInMeters currentCoordinate (Maps.LatLong lat lon))).getMeters <= hotSpotRadius
                        Nothing -> False
              )
              possibleGeoHashes
       in allPreciseHotSpot hotSpotRadius (maxLen - 1) currentCoordinate possibleGeoHashesWithInRadius
    else allPossibleGeoHashes

filterAccordingMaxFrequency :: Int -> [HotSpot] -> [HotSpot]
filterAccordingMaxFrequency threshold =
  filter
    ( \HotSpot {..} -> do
        let sumOfFrequency = _manualMovedPickup + _nonManualMovedPickup + _manualMovedSaved + _nonManualMovedSaved + _tripStart + _tripEnd + _specialLocation
        sumOfFrequency >= threshold
    )

groupAndFilterHotSpotWithPrecision :: (CacheFlow m r, EsqDBFlow m r) => Int -> Int -> [HotSpot] -> m [HotSpot]
groupAndFilterHotSpotWithPrecision precision geohashPerGroup geohashes = do
  let grouped = Dl.groupBy (\gh1 gh2 -> Dl.take precision (Dt.unpack gh1._geoHash) == Dl.take precision (Dt.unpack gh2._geoHash)) geohashes
      selected = concatMap (Dl.take geohashPerGroup) grouped
  logInfo $ "hotspot groupAndFilterWithPrecision : " <> show grouped
  pure selected

getHotspot ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Maps.LatLong ->
  Id Merchant.Merchant ->
  m HotSpotResponse
getHotspot Maps.LatLong {..} merchantId = do
  mbHotSpotConfig <- QHotSpotConfig.findConfigByMerchantId merchantId
  case mbHotSpotConfig of
    (Just HotSpotConfig {..}) ->
      if shouldTakeHotSpot
        then do
          let geoHashOfLatLong = DG.encode precisionToGetGeohash (lat, lon)
          case geoHashOfLatLong of
            Just currentGeoHash -> do
              let neighbourGeoHashes = neighboringGeohashes currentGeoHash hotSpotRadius (geoHashCellHeight precisionToGetGeohash)
              filteredHotSpots :: [HotSpot] <- concat <$> mapMaybeM (Hedis.hGet makeHotSpotKey . Dt.pack) neighbourGeoHashes
              let finalHotSpot = filterAccordingMaxFrequency minFrequencyOfHotSpot filteredHotSpots
              let sortedHotSpotWithFrequency = Dl.sortOn (Down . (\x -> do (x._manualMovedSaved * weightOfManualSaved) + (x._manualMovedPickup * weightOfManualPickup) + (x._nonManualMovedPickup * weightOfAutoPickup) + (x._nonManualMovedSaved * weightOfAutoSaved) + (x._tripStart * weightOfTripStart) + (x._tripEnd * weightOfTripEnd) + (x._specialLocation * weightOfSpecialLocation))) finalHotSpot
              filteredHotSpotWithPrecisions <- groupAndFilterHotSpotWithPrecision precisionToFilterGeohash maxGeoHashToFilter sortedHotSpotWithFrequency
              let hotSpots = take maxNumHotSpotsToShow filteredHotSpotWithPrecisions
              let hotSpotInfo = map (\HotSpot {..} -> HotSpotInfo {..}) hotSpots
              return HotSpotResponse {blockRadius = Just blockRadius, ..}
            Nothing ->
              return HotSpotResponse {hotSpotInfo = [], blockRadius = Just blockRadius, ..}
        else return HotSpotResponse {hotSpotInfo = [], blockRadius = Nothing, ..}
    Nothing -> return HotSpotResponse {hotSpotInfo = [], blockRadius = Nothing, ..}

-- Function to find all neighboring geohashes at the same precision

geoHashCellHeight :: Int -> Double
geoHashCellHeight precision =
  case precision of
    1 -> 5009000.0
    2 -> 626150.0
    3 -> 156500.0
    4 -> 19550.0
    5 -> 4890.0
    6 -> 610.0
    7 -> 153.0
    8 -> 19.1
    9 -> 4.77
    10 -> 0.596
    11 -> 0.149
    12 -> 0.0186
    _ -> 0.0

neighboringGeohashes :: String -> Double -> Double -> [String]
neighboringGeohashes geohash distance distanceThreshold = do
  let currentGeoHash :: Maybe (Double, Double) = DG.decode geohash
  let precision = length geohash
  case currentGeoHash of
    Just (lat, lon) -> do
      let neighbours = getLatlongsAtDistance (Maps.LatLong lat lon) distance distanceThreshold
      let geoHashes = mapMaybe (\neighbour -> DG.encode precision (neighbour.lat, neighbour.lon)) (Maps.LatLong lat lon : neighbours)
      Dl.nub geoHashes
    Nothing -> []

getLatlongsAtDistance :: Maps.LatLong -> Double -> Double -> [Maps.LatLong]
getLatlongsAtDistance origin distance distanceThreshold =
  let bearings = [0, 45, 90, 135, 180, 225, 270, 315]
   in [getPointAtDistance origin distance' bearing | distance' <- [0, distanceThreshold .. distance], bearing <- bearings]

getPointAtDistance :: Maps.LatLong -> Double -> Double -> Maps.LatLong
getPointAtDistance Maps.LatLong {..} distance bearing =
  let earthRadius = 6371000
      toRadians deg = deg * pi / 180
      toDegrees rad = rad * 180 / pi
      lat1Rad = toRadians lat
      lon1Rad = toRadians lon
      aRad = toRadians bearing
      lat2Rad = asin (sin lat1Rad * cos (distance / earthRadius) + cos lat1Rad * sin (distance / earthRadius) * cos aRad)
      lon2Rad =
        lon1Rad
          + atan2
            (sin aRad * sin (distance / earthRadius) * cos lat1Rad)
            (cos (distance / earthRadius) - sin lat1Rad * sin lat2Rad)
   in Maps.LatLong (toDegrees lat2Rad) (toDegrees lon2Rad)
