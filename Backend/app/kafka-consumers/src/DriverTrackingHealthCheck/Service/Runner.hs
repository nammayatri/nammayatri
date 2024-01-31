{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module DriverTrackingHealthCheck.Service.Runner where

import Consumer.LocationUpdate.Processor
import Consumer.LocationUpdate.Types (DriverIdTokenKey)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX hiding (getCurrentTime)
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified DriverTrackingHealthCheck.API as HC
import Environment (Flow, HealthCheckAppCfg)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Service
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
    drivers <- getAllDrivers locationDelay now
    case nonEmpty drivers of
      Just allDrivers -> do
        mapM_ (flip driverDevicePingService fcmNofificationSendCount) allDrivers
        log INFO ("Drivers to ping: " <> show (length allDrivers))
      Nothing -> log INFO "No drivers to ping"
    threadDelay (secondsToMcs serviceInterval).getMicroseconds

redisKey :: Text -> Text
redisKey driverId = "beckn:driver-tracking-healthcheck:drivers-to-ping:" <> driverId

driverDevicePingService :: Text -> Int -> Flow ()
driverDevicePingService driverId fcmNofificationSendCount = do
  log INFO "Ping driver"
  driver <- SQP.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
  mbCount :: Maybe Int <- Redis.safeGet $ redisKey driverId
  case mbCount of
    Just count -> do
      if count <= fcmNofificationSendCount
        then do
          void $ Redis.incr (redisKey driverId)
          pingDriver driver
        else do
          let encodedVal = A.encode $ createDriverIdTokenKey driverId
          _ <- Redis.zRem "driver-last-location-update" [T.decodeUtf8 $ BSL.toStrict encodedVal]
          _ <- Redis.del (redisKey driverId)
          pure ()
    Nothing -> do
      void $ Redis.incr (redisKey driverId)
      Redis.expire (redisKey driverId) 86400
      pingDriver driver
  where
    pingDriver :: DP.Person -> Flow ()
    pingDriver driver = do
      case driver.deviceToken of
        Just token -> notifyDevice driver.merchantOperatingCityId FCM.TRIGGER_SERVICE "You were inactive" "Please check the app" driver (Just token)
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

utcToDouble :: UTCTime -> Double
utcToDouble = realToFrac . utcTimeToPOSIXSeconds
