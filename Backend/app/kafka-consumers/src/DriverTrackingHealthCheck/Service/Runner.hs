{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DriverTrackingHealthCheck.Service.Runner where

-- import Consumer.LocationUpdate.Processor
import Consumer.LocationUpdate.Types (DriverIdTokenKey)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import qualified "dynamic-offer-driver-app" Domain.Types.Common as DriverInfo
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified DriverTrackingHealthCheck.API as HC
import Environment (Flow, HealthCheckAppCfg)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Service
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverInformation as DI
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as SQP
import "dynamic-offer-driver-app" Tools.Notifications

driverTrackingHealthcheckService :: HealthCheckAppCfg -> Flow ()
driverTrackingHealthcheckService heathCheckConfig = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService heathCheckConfig

driverLastLocationUpdateCheckService :: HealthCheckAppCfg -> Flow ()
driverLastLocationUpdateCheckService healthCheckAppCfg = startService "driverLastLocationUpdateCheckService" $ withRandomId do
  let locationDelay = healthCheckAppCfg.driverAllowedDelayForLocationUpdateInSec
      serviceInterval = healthCheckAppCfg.driverLocationHealthCheckIntervalInSec
      fcmNofificationSendCount = healthCheckAppCfg.fcmNofificationSendCount
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive
    key <- incrementCounterAndReturnShard
    drivers <- getDriversBatchFromKey key locationDelay now
    case nonEmpty drivers of
      Just allDrivers -> do
        results <- mapM (flip driverDevicePingService fcmNofificationSendCount) (toList allDrivers)
        let memberScores = catMaybes results
        void $ Redis.zRem key memberScores
        log INFO ("Drivers to ping: " <> show allDrivers)
      Nothing -> log INFO "No drivers to ping"
    threadDelay (secondsToMcs serviceInterval).getMicroseconds

redisKey :: Text -> Text
redisKey driverId = "beckn:driver-tracking-healthcheck:drivers-to-ping:" <> driverId

driverDevicePingService :: Text -> Int -> Flow (Maybe Text)
driverDevicePingService driverId fcmNofificationSendCount = do
  log INFO "Ping driver"
  SQP.findById (Id driverId) >>= \case
    Nothing -> log ERROR ("Driver not found: " <> driverId) >> pure (Just $ T.decodeUtf8 $ BSL.toStrict $ A.encode driverId)
    Just driver ->
      Redis.safeGet (redisKey driverId) >>= \case
        Just count | count > fcmNofificationSendCount -> do
          Redis.del (redisKey driverId)
          pure $ Just $ T.decodeUtf8 $ BSL.toStrict $ A.encode driverId
        _ -> Redis.incr (redisKey driverId) >> Redis.expire (redisKey driverId) 86400 >> pingDriver driver >> pure Nothing
  where
    pingDriver :: DP.Person -> Flow ()
    pingDriver driver = do
      DI.findByPrimaryKey (Id driverId) >>= \case
        Nothing -> log ERROR ("Driver information not found: " <> driverId)
        Just driverInformation ->
          case driver.deviceToken of
            Just token | driverInformation.mode `elem` [Just DriverInfo.ONLINE, Just DriverInfo.SILENT] -> notifyDevice driver.merchantOperatingCityId FCM.TRIGGER_SERVICE "You were inactive" "Please check the app" driver (Just token)
            Just _ -> log INFO $ "Driver went offline " <> show driver.id
            Nothing -> log INFO $ "Active drivers with no token" <> show driver.id

withLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m () -> m ()
withLock serviceName func =
  Redis.withLockRedis key 10 (func `catch` (logError . makeLogSomeException))
  where
    key = "beckn:" <> serviceName <> ":lock"

getAllDrivers :: (Redis.HedisFlow m r, MonadReader r m) => Seconds -> UTCTime -> m [Text]
getAllDrivers locationDelay now = do
  let presentTime = negate (fromIntegral locationDelay) `addUTCTime` now
  redisRes <- Redis.withCrossAppRedis $ Redis.zRangeByScore "driver-last-location-update" 0 $ utcToDouble presentTime
  let mbDecodedVal = map decode redisRes
      decodedVal = catMaybes mbDecodedVal
      res = map (.driverId) decodedVal
  pure res
  where
    decode :: BS.ByteString -> Maybe DriverIdTokenKey
    decode val = do
      let res = A.decode $ BSL.fromStrict val
      res

getDriversBatchFromKey :: Text -> Seconds -> UTCTime -> Flow [Text]
getDriversBatchFromKey key locationDelay now = do
  let presentTime = negate (fromIntegral locationDelay) `addUTCTime` now
  batchSize <- fromMaybe 100 . fmap (.batchSize) <$> asks (.healthCheckAppCfg)
  redisRes <- Redis.withCrossAppRedis $ Redis.zRangeByScoreByCount key 0 (utcToDouble presentTime) 0 batchSize
  pure $ mapMaybe decode redisRes
  where
    decode :: BS.ByteString -> Maybe Text
    decode val = do
      let res = A.decode $ BSL.fromStrict val
      res

incrementCounterAndReturnShard :: Flow Text
incrementCounterAndReturnShard = do
  numberOfShards <- fromMaybe 10 . fmap (.numberOfShards) <$> asks (.healthCheckAppCfg)
  getKeyWithShard . (`mod` numberOfShards) <$> Redis.incr incrementCountKey

incrementCountKey :: Text
incrementCountKey = "driver-location-consume-batch-count"

getKeyWithShard :: Integer -> Text
getKeyWithShard shardNo = "driver-last-location-update-{shard-" <> show shardNo <> "}"

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds
