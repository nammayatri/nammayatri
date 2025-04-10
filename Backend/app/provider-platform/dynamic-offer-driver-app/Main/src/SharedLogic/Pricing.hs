{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Pricing where

import Data.Text as T hiding (elem, find, length, null)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Tools.Maps

getDistanceBin :: Int -> Text
getDistanceBin distance
  | distance < 0 = "unknown"
  | distance < 2000 = "0-2"
  | distance < 4000 = "2-4"
  | distance < 6000 = "4-6"
  | distance < 8000 = "6-8"
  | distance < 10000 = "8-10"
  | distance < 12000 = "10-12"
  | distance < 14000 = "12-14"
  | distance < 16000 = "14-16"
  | distance < 18000 = "16-18"
  | distance < 20000 = "18-20"
  | otherwise = "more_than_20"

-- Function to get the start of the 15-minute interval for a given time
getBucketStart :: UTCTime -> UTCTime
getBucketStart time =
  let -- Get the number of seconds since midnight
      secondsFromMidnight = getSecondsFromMidnight time
      -- Round down to nearest 15-minute mark (900 seconds = 15 minutes)
      intervalStart = (secondsFromMidnight `div` 900) * 900
      -- Return the UTCTime with the start of the interval
      (UTCTime day _) = time
   in UTCTime day (fromIntegral intervalStart)

-- Function to build the Redis key for a 15-minute interval
buildTimeBucket :: UTCTime -> T.Text
buildTimeBucket time = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" (getBucketStart time)

getSecondsFromMidnight :: UTCTime -> Integer
getSecondsFromMidnight (UTCTime _ diffTime) = floor diffTime

parseTimeString :: String -> Maybe UTCTime
parseTimeString str = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" str :: Maybe UTCTime

mkDemandVehicleCategoryCity :: UTCTime -> Maybe DVC.VehicleCategory -> Text -> Text
mkDemandVehicleCategoryCity time vehicleCategory cityId = "demand_VC_" <> show vehicleCategory <> "_cityId_" <> cityId <> "_time_" <> (buildTimeBucket time)

mkAcceptanceVehicleCategoryCity :: UTCTime -> Maybe DVC.VehicleCategory -> Text -> Text
mkAcceptanceVehicleCategoryCity time vehicleCategory cityId = "acceptance_VC_" <> show vehicleCategory <> "_cityId_" <> cityId <> "_time_" <> (buildTimeBucket time)

mkDemandVehicleCategory :: UTCTime -> Maybe DVC.VehicleCategory -> Text
mkDemandVehicleCategory time vehicleCategory = "demand_VC_" <> show vehicleCategory <> "_time_" <> (buildTimeBucket time)

mkAcceptanceVehicleCategory :: UTCTime -> Maybe DVC.VehicleCategory -> Text
mkAcceptanceVehicleCategory time vehicleCategory = "acceptance_VC_" <> show vehicleCategory <> "_time_" <> (buildTimeBucket time)

mkDemandVehicleCategoryWithDistanceBin :: UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text
mkDemandVehicleCategoryWithDistanceBin time vehicleCategory (Just distance) = "demand_VC_" <> show vehicleCategory <> "_dB_" <> (getDistanceBin distance) <> "_time_" <> (buildTimeBucket time)
mkDemandVehicleCategoryWithDistanceBin time vehicleCategory Nothing = mkDemandVehicleCategory time vehicleCategory

mkAcceptanceVehicleCategoryWithDistanceBin :: UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text
mkAcceptanceVehicleCategoryWithDistanceBin time vehicleCategory (Just distance) = "acceptance_VC_" <> show vehicleCategory <> "_dB_" <> (getDistanceBin distance) <> "_time_" <> (buildTimeBucket time)
mkAcceptanceVehicleCategoryWithDistanceBin time vehicleCategory Nothing = mkAcceptanceVehicleCategory time vehicleCategory

mkCongestionVehicleCategory :: UTCTime -> Maybe DVC.VehicleCategory -> Text
mkCongestionVehicleCategory time vehicleCategory = "congestion_VC_" <> show vehicleCategory <> "_time_" <> (buildTimeBucket time)

mkCongestionVehicleCategoryWithDistanceBin :: UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text
mkCongestionVehicleCategoryWithDistanceBin time vehicleCategory (Just distance) = "congestion_VC_" <> show vehicleCategory <> "_dB_" <> (getDistanceBin distance) <> "_time_" <> (buildTimeBucket time)
mkCongestionVehicleCategoryWithDistanceBin time vehicleCategory Nothing = mkCongestionVehicleCategory time vehicleCategory

getQARWithDistance ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  UTCTime ->
  Maybe DVC.VehicleCategory ->
  Int ->
  Double ->
  LatLong ->
  m (Maybe Double)
getQARWithDistance time vehicleCategory distance radius location = do
  let demandKey = mkDemandVehicleCategoryWithDistanceBin time vehicleCategory (Just distance)
      timeN_1 = addUTCTime (-900) time --------Previous time bucket
      demandKeyN_1 = mkDemandVehicleCategoryWithDistanceBin timeN_1 vehicleCategory (Just distance)
  demandDataN <- Hedis.withCrossAppRedis $ Hedis.geoSearch demandKey (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
  demandDataN_1 <- Hedis.withCrossAppRedis $ Hedis.geoSearch demandKeyN_1 (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
  let demandData = demandDataN ++ demandDataN_1
  if length demandData > 4
    then do
      let acceptanceKey = mkAcceptanceVehicleCategoryWithDistanceBin time vehicleCategory (Just distance)
          acceptanceKeyN_1 = mkAcceptanceVehicleCategoryWithDistanceBin timeN_1 vehicleCategory (Just distance)
      acceptanceDataN <- Hedis.withCrossAppRedis $ Hedis.geoSearch acceptanceKey (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
      acceptanceDataN_1 <- Hedis.withCrossAppRedis $ Hedis.geoSearch acceptanceKeyN_1 (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
      let acceptanceData = acceptanceDataN ++ acceptanceDataN_1
      return $ Just $ fromIntegral (length acceptanceData) / fromIntegral (length demandData)
    else return Nothing

getQARVehicleCategory ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  UTCTime ->
  Maybe DVC.VehicleCategory ->
  Double ->
  LatLong ->
  m (Maybe Double)
getQARVehicleCategory time vehicleCategory radius location = do
  let demandKey = mkDemandVehicleCategory time vehicleCategory
      timeN_1 = addUTCTime (-900) time --------Previous time bucket
      demandKeyN_1 = mkDemandVehicleCategory timeN_1 vehicleCategory
  demandDataN <- Hedis.withCrossAppRedis $ Hedis.geoSearch demandKey (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
  demandDataN_1 <- Hedis.withCrossAppRedis $ Hedis.geoSearch demandKeyN_1 (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
  let demandData = demandDataN ++ demandDataN_1
  if length demandData > 4
    then do
      let acceptanceKey = mkAcceptanceVehicleCategory time vehicleCategory
          acceptanceKeyN_1 = mkAcceptanceVehicleCategory timeN_1 vehicleCategory
      acceptanceDataN <- Hedis.withCrossAppRedis $ Hedis.geoSearch acceptanceKey (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
      acceptanceDataN_1 <- Hedis.withCrossAppRedis $ Hedis.geoSearch acceptanceKeyN_1 (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km")
      let acceptanceData = acceptanceDataN ++ acceptanceDataN_1
      return $ Just $ fromIntegral (length acceptanceData) / fromIntegral (length demandData)
    else return Nothing

getQARVehicleCategoryCity ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  UTCTime ->
  Maybe DVC.VehicleCategory ->
  Text ->
  m (Maybe Double)
getQARVehicleCategoryCity time vehicleCategory cityId = do
  let demandKey = mkDemandVehicleCategoryCity time vehicleCategory cityId
      timeN_1 = addUTCTime (-900) time
      demandKeyN_1 = mkDemandVehicleCategoryCity timeN_1 vehicleCategory cityId
  demandDataN :: Maybe Int <- Hedis.withCrossAppRedis $ Hedis.get demandKey
  demandDataN_1 :: Maybe Int <- Hedis.withCrossAppRedis $ Hedis.get demandKeyN_1
  let demandData = fromMaybe 0 demandDataN + fromMaybe 0 demandDataN_1
  if demandData > 4
    then do
      let acceptanceKey = mkAcceptanceVehicleCategoryCity time vehicleCategory cityId
          acceptanceKeyN_1 = mkAcceptanceVehicleCategoryCity timeN_1 vehicleCategory cityId
      acceptanceDataN :: Maybe Int <- Hedis.withCrossAppRedis $ Hedis.get acceptanceKey
      acceptanceDataN_1 :: Maybe Int <- Hedis.withCrossAppRedis $ Hedis.get acceptanceKeyN_1
      let acceptanceData = fromMaybe 0 acceptanceDataN + fromMaybe 0 acceptanceDataN_1
      return $ Just $ fromIntegral acceptanceData / fromIntegral demandData
    else return Nothing
