module Services.DriverTrackingHealthcheck (driverTrackingHealthcheckService) where

import App.DriverTrackingHealthcheck.Environment (AppEnv)
import Beckn.External.FCM.Types (FCMNotificationType (..))
import Beckn.Prelude
import Beckn.Storage.Redis.Queries (lpush, rpop, tryLockRedis, unlockRedis)
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import qualified Product.HealthCheck as HC
import qualified Storage.Queries.DriverInformation as DrInfo
import Utils.Common
import Utils.Notifications (notifyDriver)

type Flow = FlowR AppEnv

-- TODO: move this function somewhere
withLock :: Text -> Flow () -> Flow ()
withLock serviceName f = do
  _ <- getLock
  HC.iAmAlive serviceName
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
driverLastLocationUpdateCheckService = service "driverLastLocationUpdateCheckService" do
  delay <- askConfig (.driverAllowedDelay)
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
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

redisKey :: Text
redisKey = "beckn:driver-tracking-healthcheck:drivers-to-ping"

driverDevicePingService :: Flow ()
driverDevicePingService = service "driverDevicePingService" do
  rpop redisKey >>= flip whenJust \case
    (Id driverId, Nothing) ->
      log ERROR $ "Driver " <> driverId <> " is active, but has no device token"
    (driverId, token) -> do
      notifyDriver PING "You were inactive" "Please check the app" driverId token
      log DEBUG $ "Pinged " <> driverId.getId
  askConfig (.notificationMinDelay)
    >>= threadDelay . (.getMicroseconds)
