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
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import Storage.CachedQueries.Maps.LocationMapCache
import Tools.Auth
import qualified Tools.Maps as Maps

type API = "getHotSpot" :> TokenAuth :> ReqBody '[JSON] Maps.LatLong :> Get '[JSON] HotSpotResponse

handler :: FlowServer API
handler = getHotSpot

getHotSpot :: (Id Person.Person, Id Merchant.Merchant) -> Maps.LatLong -> FlowHandler HotSpotResponse
getHotSpot (_, merchantId) latlong = withFlowHandlerAPI (getHotspot latlong merchantId)

-- hotspot
allPreciseHotSpot :: Int -> [String]
allPreciseHotSpot maxLen = [1 .. maxLen] >>= \len -> replicateM len "0123456789bcdefghjkmnpqrstuvwxyz"

filterAccordingMaxFrequency :: Int -> [HotSpot] -> [HotSpot]
filterAccordingMaxFrequency threshold =
  filter
    ( \HotSpot {..} -> do
        let sumOfFrequency = _manualMovedPickup + _nonManualMovedPickup + _manualMovedSaved + _nonManualMovedSaved + _tripStart + _tripEnd + _specialLocation
        sumOfFrequency >= threshold
    )

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
          let mbGeoHashOfLatLong = DG.encode nearbyGeohashPrecision (lat, lon)
          case mbGeoHashOfLatLong of
            Just geoHashOfLatLong -> do
              let neighbours = neighboringGeohashes geoHashOfLatLong
              let allGeoHashes = Dl.nub (geoHashOfLatLong : neighbours)
              let deviatedCharLen = hotSpotGeoHashPrecision - nearbyGeohashPrecision
              let listOfGeoHashToCheck =
                    concatMap
                      ( \ghash -> do
                          let addedCharacters = allPreciseHotSpot deviatedCharLen
                          let listOfStringsAddedToGeohash = map (ghash ++) addedCharacters
                          listOfStringsAddedToGeohash
                      )
                      allGeoHashes
              filteredAccordingToGeoHash :: [HotSpot] <- mapMaybeM (Hedis.hGet makeHotSpotKey . Dt.pack) listOfGeoHashToCheck
              let finalHotSpot = filterAccordingMaxFrequency minFrequencyOfHotSpot filteredAccordingToGeoHash
              let hotSpots = take maxNumHotSpotsToShow $ Dl.sortOn (Down . (\x -> do (x._manualMovedSaved * weightOfManualSaved) + (x._manualMovedPickup * weightOfManualPickup) + (x._nonManualMovedPickup * weightOfAutoPickup) + (x._nonManualMovedSaved * weightOfAutoSaved) + (x._tripStart * weightOfTripStart) + (x._tripEnd * weightOfTripEnd) + (x._specialLocation * weightOfSpecialLocation))) finalHotSpot
              let hotSpotInfo = map (\HotSpot {..} -> HotSpotInfo {..}) hotSpots
              return HotSpotResponse {blockRadius = Just blockRadius, ..}
            Nothing ->
              return HotSpotResponse {hotSpotInfo = [], blockRadius = Just blockRadius, ..}
        else return HotSpotResponse {hotSpotInfo = [], blockRadius = Nothing, ..}
    Nothing -> return HotSpotResponse {hotSpotInfo = [], blockRadius = Nothing, ..}

-- Function to find all neighboring geohashes at the same precision

geoHashLengthToLatLongCentroidalDistance :: Int -> Double
geoHashLengthToLatLongCentroidalDistance geoHashLength = case geoHashLength of
  1 -> 5009000
  2 -> 1252300
  3 -> 156500
  4 -> 39100
  5 -> 4900
  6 -> 1200
  7 -> 152
  8 -> 38
  9 -> 5
  10 -> 1
  11 -> 0.14
  12 -> 0.03
  _ -> 0

neighboringGeohashes :: String -> [String]
neighboringGeohashes geohash = do
  let mbCentroid :: Maybe (Double, Double) = DG.decode geohash
  let precision = length geohash
  let distance = geoHashLengthToLatLongCentroidalDistance precision
  case mbCentroid of
    Just (lat, lon) -> do
      let neighbours = getLatlongsAtDistance (Maps.LatLong lat lon) distance
      let geoHashes = mapMaybe (\neighbour -> DG.encode precision (neighbour.lat, neighbour.lon)) neighbours
      geoHashes
    Nothing -> []

getLatlongsAtDistance :: Maps.LatLong -> Double -> [Maps.LatLong]
getLatlongsAtDistance origin distance =
  let bearings = [0, 45, 90, 135, 180, 225, 270, 315]
      toRadians deg = deg * pi / 180
      toDegrees rad = rad * 180 / pi
      originRadians = Maps.LatLong (toRadians $ origin.lat) (toRadians $ origin.lon)
      latLongs = [getPointAtDistance originRadians distance (toRadians bearing) | bearing <- bearings]
   in [Maps.LatLong (toDegrees $ latLong.lat) (toDegrees $ latLong.lon) | latLong <- latLongs]

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
