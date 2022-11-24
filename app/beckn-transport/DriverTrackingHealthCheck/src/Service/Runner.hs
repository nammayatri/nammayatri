{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Service.Runner where

import qualified API as HC
import Beckn.External.Encryption (decrypt)
import Beckn.External.FCM.Types (FCMNotificationType (TRIGGER_SERVICE))
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis (lPush, rPop)
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Common
import Beckn.Types.Error (PersonError (PersonFieldNotPresent))
import Beckn.Types.Id (Id, cast)
import Beckn.Utils.Common
import Beckn.Utils.Service
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import Domain.Types.Person (Driver)
import qualified Domain.Types.Person as SP
import Environment (Flow)
import qualified Storage.Queries.DriverInformation as DrInfo
import Tools.Notifications

driverTrackingHealthcheckService :: Flow ()
driverTrackingHealthcheckService = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService
  driverDevicePingService
  driverMakingInactiveService

driverLastLocationUpdateCheckService :: Flow ()
driverLastLocationUpdateCheckService = startService "driverLastLocationUpdateCheckService" $ withRandomId do
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
            lPush redisKey driversWithToken
            logPretty INFO ("Drivers to ping: " <> show (length driversWithToken)) driversWithToken
          Nothing -> log INFO "No drivers to ping"
  threadDelay (secondsToMcs delay).getMicroseconds
  where
    fetchPersonIdAndMobileNumber :: SP.Person -> (Id Driver, Maybe FCM.FCMRecipientToken)
    fetchPersonIdAndMobileNumber driver = (cast driver.id :: Id Driver, driver.deviceToken)

redisKey :: Text
redisKey = "beckn:driver-tracking-healthcheck:drivers-to-ping"

driverDevicePingService :: Flow ()
driverDevicePingService = startService "driverDevicePingService" do
  HC.iAmAlive
  rPop redisKey >>= flip whenJust \(driverId, token) ->
    withLogTag driverId.getId do
      log INFO "Ping driver"
      notifyDevice TRIGGER_SERVICE "You were inactive" "Please check the app" driverId (Just token)
  asks (.notificationMinDelay)
    >>= threadDelay . (.getMicroseconds)

driverMakingInactiveService :: Flow ()
driverMakingInactiveService = startService "driverMakingInactiveService" $ withRandomId do
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

withLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m () -> m ()
withLock serviceName func =
  Redis.withLockRedis key 10 (func `catch` (logError . makeLogSomeException))
  where
    key = "beckn:" <> serviceName <> ":lock"
