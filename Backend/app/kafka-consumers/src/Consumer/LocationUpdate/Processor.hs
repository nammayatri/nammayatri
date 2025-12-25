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
import qualified Data.HashMap.Strict as HM
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.UUID as UU
import Environment
import EulerHS.Prelude hiding (toStrict)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Logging (logDebug)

processLocationData :: [Text] -> [(T.LocationUpdates, T.DriverId)] -> Flow ()
processLocationData enabledMerchantCityIds locationData = do
  logDebug $ "driver updated time locationData: " <> show locationData
  let encodedVals =
        mapFilter
          (\(T.LocationUpdates {..}, driverId) -> (utcToDouble ts, driverId, idToIdHashNumber driverId))
          (\(T.LocationUpdates {..}, _) -> (fromMaybe "" mocid) `elem` enabledMerchantCityIds)
          locationData
  let encodeValsInShardMap =
        foldl
          ( \acc (ts, dId, dIdHashNumber) -> do
              let updatedDIdHashNumberList =
                    case HM.lookup dIdHashNumber acc of
                      Just driverShardList -> (ts, dId) : driverShardList
                      Nothing -> [(ts, dId)]
              HM.insert dIdHashNumber updatedDIdHashNumberList acc
          )
          HM.empty
          encodedVals
  encodeValsWithShardKey <- mapM (\(dIdHashNumber, val) -> (,val) <$> getKeyWithShard dIdHashNumber) (HM.toList encodeValsInShardMap)
  void $ mapM (\(key, val) -> Redis.zAdd key val) encodeValsWithShardKey
  where
    mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
    mapFilter mapFunc filterFunc xs = [mapFunc x | x <- xs, filterFunc x]

idToIdHashNumber :: Text -> Integer
idToIdHashNumber uuidTxt = fromMaybe 0 $ fromIntegral . ((\(a, b, c, d) -> a + b + c + d) . UU.toWords) <$> UU.fromText uuidTxt

getKeyWithShard :: Integer -> Flow Text
getKeyWithShard dIdHashNumber = do
  numberOfShards <- fromMaybe 10 . fmap (.numberOfShards) <$> asks (.healthCheckAppCfg)
  pure $ makeShardKey (dIdHashNumber `mod` numberOfShards)

makeShardKey :: Integer -> Text
makeShardKey shardNo = "driver-last-location-update-{shard-" <> show shardNo <> "}"

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds
