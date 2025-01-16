{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Consumer.LocationUpdate.Processor
  ( processLocationData,
  )
where

import qualified Consumer.AvailabilityTime.Types as T
import Data.Time
import Data.Time.Clock.POSIX
import Environment
import EulerHS.Prelude hiding (toStrict)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Logging (logDebug)

processLocationData :: [Text] -> [(T.LocationUpdates, T.DriverId)] -> Flow ()
processLocationData enabledMerchantCityIds locationData = do
  logDebug $ "driver updated time locationData: " <> show locationData
  key <- incrementCounterAndReturnShard
  let encodedVals =
        mapFilter
          (\(T.LocationUpdates {..}, driverId) -> (utcToDouble ts, driverId))
          (\(T.LocationUpdates {..}, _) -> (fromMaybe "" mocid) `elem` enabledMerchantCityIds)
          locationData
  Redis.zAdd key encodedVals
  where
    mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
    mapFilter mapFunc filterFunc xs = [mapFunc x | x <- xs, filterFunc x]

incrementCounterAndReturnShard :: Flow Text
incrementCounterAndReturnShard = do
  numberOfShards <- fromMaybe 10 . fmap (.numberOfShards) <$> asks (.healthCheckAppCfg)
  getKeyWithShard . (`mod` numberOfShards) <$> Redis.incr incrementCountKey

incrementCountKey :: Text
incrementCountKey = "driver-location-update-batch-count"

getKeyWithShard :: Integer -> Text
getKeyWithShard shardNo = "driver-last-location-update-{shard-" <> show shardNo <> "}"

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds
