{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module DriverTrackingHealthCheck.Service.Runner where

import Beckn.External.Encryption (decrypt)
import Beckn.External.FCM.Types (FCMNotificationType (TRIGGER_SERVICE))
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Redis.Queries (lpush, rpop)
import Beckn.Types.Common
import Beckn.Types.Error (PersonError (PersonFieldNotPresent))
import Beckn.Types.Id (Id, cast)
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.Person as SP
import qualified DriverTrackingHealthCheck.API as HC
import DriverTrackingHealthCheck.Environment (Flow)
import DriverTrackingHealthCheck.Service.Runner.DataLocker
import qualified Storage.Queries.DriverInformation as DrInfo
import Types.App (Driver)
import Utils.Common
import Utils.Notifications (notifyDevice)

driverTrackingHealthcheckService :: Flow ()
driverTrackingHealthcheckService = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService
  driverDevicePingService
  driverMakingInactiveService

driverLastLocationUpdateCheckService :: Flow ()
driverLastLocationUpdateCheckService = service "driverLastLocationUpdateCheckService" $ withRandomId do
  delay <- asks (.driverAllowedDelay)
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive
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

redisKey :: Text
redisKey = "beckn:driver-tracking-healthcheck:drivers-to-ping"

driverDevicePingService :: Flow ()
driverDevicePingService = service "driverDevicePingService" do
  HC.iAmAlive
  rpop redisKey >>= flip whenJust \(driverId, token) ->
    withLogTag driverId.getId do
      log INFO "Ping driver"
      notifyDevice TRIGGER_SERVICE "You were inactive" "Please check the app" driverId (Just token)
  asks (.notificationMinDelay)
    >>= threadDelay . (.getMicroseconds)

driverMakingInactiveService :: Flow ()
driverMakingInactiveService = service "driverMakingInactiveService" $ withRandomId do
  delay <- asks (.driverInactiveDelay)
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverMakingInactiveService" do
    now <- getCurrentTime
    HC.iAmAlive
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
