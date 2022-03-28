module Services.DriverTrackingHealthcheck where

import App.DriverTrackingHealthcheck.Environment (AppEnv)
import Beckn.External.FCM.Types (FCMNotificationType (..))
import Beckn.Prelude
import Beckn.Storage.Redis.Queries (lpush, rpop, tryLockRedis, unlockRedis)
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import qualified Product.HealthCheck as HC
import qualified Storage.Queries.DriverInformation as DrInfo
import Utils.Common
import Utils.Notifications (notifyDevice)

type Flow = FlowR AppEnv

-- TODO: move this function somewhere
withLock :: Flow () -> Flow ()
withLock f = do
  getLock
  f `catch` (log ERROR . makeLogSomeException)
  unlockRedis lockKey
  where
    getLock = do
      lockAvailable <- tryLockRedis lockKey 10
      unless lockAvailable getLock
    lockKey = "beckn:" <> serviceName <> ":lock"

driverTrackingHealthcheckService :: Flow ()
driverTrackingHealthcheckService = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService
  driverDevicePingService

driverLastLocationUpdateCheckService :: Flow ()
driverLastLocationUpdateCheckService = service "driverLastLocationUpdateCheckService" $ withRandomId do
  delay <- asks (.driverAllowedDelay)
  withLock $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive serviceName
    DrInfo.getDriversWithOutdatedLocations (negate (fromIntegral delay) `addUTCTime` now)
      <&> map \case
        (driverId, Nothing) -> Left driverId
        (driverId, Just token) -> Right (driverId, token)
      <&> partitionEithers
      >>= \(noTokenIds, driversToPing) -> do
        unless (null noTokenIds) $ logPretty ERROR "Active drivers with no token" noTokenIds
        case nonEmpty driversToPing of
          Just drivers -> do
            lpush redisKey drivers
            logPretty INFO ("Drivers to ping: " <> show (length drivers)) drivers
          Nothing -> log INFO "No drivers to ping"
  threadDelay (secondsToMcs delay).getMicroseconds

serviceName :: Text
serviceName = "driver-tracking-healthcheck"

redisKey :: Text
redisKey = "beckn:driver-tracking-healthcheck:drivers-to-ping"

driverDevicePingService :: Flow ()
driverDevicePingService = service "driverDevicePingService" do
  HC.iAmAlive serviceName
  rpop redisKey >>= flip whenJust \(driverId, token) ->
    withLogTag driverId.getId do
      log INFO "Ping driver"
      notifyDevice TRIGGER_SERVICE "You were inactive" "Please check the app" driverId (Just token)
  asks (.notificationMinDelay)
    >>= threadDelay . (.getMicroseconds)
