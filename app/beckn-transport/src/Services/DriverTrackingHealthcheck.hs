{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Services.DriverTrackingHealthcheck where

import App.DriverTrackingHealthcheck.Environment (AppEnv)
import Beckn.Prelude
import Beckn.Storage.Redis.Queries (lpush, rpop, tryLockRedis, unlockRedis)
import Beckn.Types.Common
import Beckn.Types.Flow (FlowR)
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import qualified Product.HealthCheck as HC
import qualified Storage.Queries.DriverInformation as DrInfo
import Utils.Common
import qualified Beckn.Storage.Esqueleto as Esq
import Utils.Notifications (notifyDevice)
import Beckn.Types.Error (PersonError(PersonFieldNotPresent))
import qualified Domain.Types.Person as SP
import Beckn.External.FCM.Types (FCMNotificationType(TRIGGER_SERVICE))
import Beckn.External.Encryption (decrypt)
import Beckn.Types.Id (cast, Id)
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.FCM.Types as FCM
import Types.App (Driver)

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
  driverMakingInactiveService

driverLastLocationUpdateCheckService :: Flow ()
driverLastLocationUpdateCheckService = service "driverLastLocationUpdateCheckService" $ withRandomId do
  delay <- asks (.driverAllowedDelay)
  withLock $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive serviceName
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral delay) `addUTCTime` now)
    let driverDetails = map fetchPersonIdAndMobileNumber drivers
    flip map driverDetails \case
        (driverId, Nothing) -> Left driverId
        (driverId, Just token) -> Right (driverId, token)
      & partitionEithers
      & \(noTokenIds, driversToPing) -> do
        unless (null noTokenIds) $ logPretty ERROR "Active drivers with no token" noTokenIds
        case nonEmpty driversToPing of
          Just driversWithToken -> do
            lpush redisKey driversWithToken
            logPretty INFO ("Drivers to ping: " <> show (length driversWithToken)) driversWithToken
          Nothing -> log INFO "No drivers to ping"
  threadDelay (secondsToMcs delay).getMicroseconds
  where
    fetchPersonIdAndMobileNumber :: SP.Person -> (Id Driver, Maybe FCM.FCMRecipientToken)
    fetchPersonIdAndMobileNumber driver = (cast driver.id :: Id Driver, driver.deviceToken)

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


driverMakingInactiveService :: Flow ()
driverMakingInactiveService = service "driverMakingInactiveService" $ withRandomId do
  delay <- asks (.driverInactiveDelay)
  withLock $ measuringDurationToLog INFO "driverMakingInactiveService" do
    now <- getCurrentTime
    HC.iAmAlive serviceName
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral delay) `addUTCTime` now)
    logPretty INFO ("Drivers to make inactive: " <> show (length drivers)) ((.id) <$> drivers)
    mapM_ fetchPersonIdAndMobileNumber drivers 
  threadDelay (secondsToMcs delay).getMicroseconds
  where
    fetchPersonIdAndMobileNumber :: SP.Person -> Flow () 
    fetchPersonIdAndMobileNumber driver = withLogTag ("driverId_" <> driver.id.getId) do
      mobileNumber' <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      log INFO "Make driver inactive"
      Esq.runTransaction $
          DrInfo.updateActivity (cast driver.id) False

      smsCfg <- asks (.smsCfg)
      driverInactiveSmsTemplate <- asks (.driverInactiveSmsTemplate)

      SF.sendSms smsCfg driverInactiveSmsTemplate (countryCode <> mobileNumber')
        >>= SF.checkSmsResult
