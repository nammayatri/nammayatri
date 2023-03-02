{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Service.Runner where

import qualified API as HC
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import Domain.Types.Person (Driver)
import qualified Domain.Types.Person as SP
import Environment (Flow)
import Kernel.External.Encryption (decrypt)
import Kernel.External.FCM.Types (FCMNotificationType (TRIGGER_SERVICE))
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.SMS.MyValueFirst.Flow as SF
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (lPush, rPop)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error (PersonError (PersonFieldNotPresent, PersonNotFound))
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import Kernel.Utils.Service
import SharedLogic.TransporterConfig
import qualified Storage.Queries.DriverInformation as DrInfo
import qualified Storage.Queries.Person as SQP
import Tools.Notifications

driverTrackingHealthcheckService :: Flow ()
driverTrackingHealthcheckService = withLogTag "driverTrackingHealthcheckService" do
  driverLastLocationUpdateCheckService
  driverDevicePingService
  driverMakingInactiveService

driverLastLocationUpdateCheckService :: Flow ()
driverLastLocationUpdateCheckService = startService "driverLastLocationUpdateCheckService" $ withRandomId do
  locationDelay <- asks (.driverAllowedDelayForLocationUpdateInSec)
  serviceInterval <- asks (.driverLocationHealthCheckIntervalInSec)
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverLastLocationUpdateCheckService" do
    now <- getCurrentTime
    HC.iAmAlive
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral locationDelay) `addUTCTime` now) (Proxy @Flow)
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
  threadDelay (secondsToMcs serviceInterval).getMicroseconds
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
      driver <- SQP.findById driverId (Proxy @Flow) >>= fromMaybeM (PersonNotFound driverId.getId)
      fcmConfig <- findFCMConfigByMerchantId driver.merchantId
      notifyDevice fcmConfig{FCM.fcmTokenKeyPrefix = "transporter-healthcheck"} TRIGGER_SERVICE "You were inactive" "Please check the app" driverId (Just token)
  asks (.notificationMinDelay)
    >>= threadDelay . (.getMicroseconds)

driverMakingInactiveService :: Flow ()
driverMakingInactiveService = startService "driverMakingInactiveService" $ withRandomId do
  delay <- asks (.driverInactiveDelay)
  withLock "driver-tracking-healthcheck" $ measuringDurationToLog INFO "driverMakingInactiveService" do
    now <- getCurrentTime
    HC.iAmAlive
    drivers <- DrInfo.getDriversWithOutdatedLocationsToMakeInactive (negate (fromIntegral delay) `addUTCTime` now) (Proxy @Flow)
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
        DrInfo.updateActivity @Flow (cast driver.id) False

      smsCfg <- asks (.smsCfg)
      driverInactiveSmsTemplate <- asks (.driverInactiveSmsTemplate)

      fork "smsServiceToMarkDriverInactive" $
        SF.sendSms smsCfg driverInactiveSmsTemplate (countryCode <> mobileNumber')
          >>= SF.checkSmsResult

withLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m () -> m ()
withLock serviceName func =
  Redis.withLockRedis key 10 (func `catch` (logError . makeLogSomeException))
  where
    key = "beckn:" <> serviceName <> ":lock"
