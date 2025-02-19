{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideNotificationsToDriver where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Domain.Action.UI.Call
import qualified Domain.Types as DTC
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Common as DInfo
import qualified Domain.Types.Overlay as DOverlay
import Domain.Types.RideRelatedNotificationConfig
import Domain.Types.TransporterConfig
import qualified Kernel.Beam.Functions as B
import Kernel.External.Call.Interface.Types
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (..), SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.Overlay as CMO
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import Tools.Call
import Tools.Error
import Tools.Notifications
import qualified Tools.SMS as Sms
import Utils.Common.Cac.KeyNameConstants

sendScheduledRideNotificationsToDriver ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    SchedulerFlow r
  ) =>
  Job 'ScheduledRideNotificationsToDriver ->
  m ExecutionResult
sendScheduledRideNotificationsToDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      bookingId = jobData.bookingId
      driverId = jobData.driverId
      notificationType = jobData.notificationType
      notificationKey = jobData.notificationKey
      onlyIfOffline = jobData.onlyIfOffline
      merchantId = jobData.merchantId
  booking <- QB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  let isNotificationRequired = not onlyIfOffline || (driverInfo.mode /= Just DInfo.ONLINE)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)

  when (isNotificationRequired && booking.status /= DB.CANCELLED) do
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    let maybeAppId = (HM.lookup RentalAppletID . exotelMap) =<< transporterConfig.exotelAppIdMapping
    driver <- B.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
    mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
    countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
    let phoneNumber = countryCode <> mobileNumber
    case notificationType of
      CALL -> do
        callStatusId <- generateGUID
        let callReq =
              InitiateCallReq
                { fromPhoneNum = phoneNumber,
                  toPhoneNum = Nothing,
                  attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = bookingId.getId},
                  appletId = maybeAppId
                }
        exotelResponse <- initiateCall booking.providerId merchantOpCityId callReq
        logTagInfo ("BookingId: " <> bookingId.getId) "IVR Call initiated to driver."
        callStatus <- buildCallStatus callStatusId exotelResponse booking
        void $ QCallStatus.create callStatus
      PN -> do
        merchantPN <- CPN.findMatchingMerchantPN merchantOpCityId notificationKey Nothing Nothing driver.language Nothing >>= fromMaybeM (MerchantPNNotFound merchantOpCityId.getId notificationKey)
        let entityData = generateReq merchantPN.title merchantPN.body booking merchant
        notifyDriverOnEvents merchantOpCityId driverId driver.deviceToken entityData merchantPN.fcmNotificationType
      OVERLAY -> do
        overlayKey <- A.decode (A.encode notificationKey) & fromMaybeM (InvalidRequest "Invalid overlay key for Notification")
        merchantOverlay <- CMO.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory merchantOpCityId overlayKey ENGLISH Nothing Nothing >>= fromMaybeM (OverlayKeyNotFound notificationKey)
        let (title, description) = formatMessageTransformer (fromMaybe "" merchantOverlay.title) (fromMaybe "" merchantOverlay.description) booking merchant.shortId
        let overlay :: DOverlay.Overlay = overlay {DOverlay.title = Just title, DOverlay.description = Just description}
        sendOverlay merchantOpCityId driver $ mkOverlayReq overlay
      SMS -> do
        smsCfg <- asks (.smsCfg)
        messageKey <- A.decode (A.encode notificationKey) & fromMaybeM (InvalidRequest "Invalid message key for SMS")
        merchantMessage <- CMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey Nothing Nothing >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId notificationKey)
        let sender = fromMaybe smsCfg.sender merchantMessage.senderHeader
            (_, messageBody) = formatMessageTransformer "" merchantMessage.message booking merchant.shortId
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq messageBody phoneNumber sender) >>= Sms.checkSmsResult
      _ -> pure () -- WHATSAPP or Other Notifications can be implemented here
  return Complete
  where
    generateReq notifTitle notifBody booking merchant = do
      let (title, message) = formatMessageTransformer notifTitle notifBody booking merchant.shortId
      NotifReq
        { title = title,
          message = message,
          entityId = booking.id.getId
        }

    formatMessageTransformer title body booking merchantShortId = do
      let isRentalOrIntercity = case booking.tripCategory of
            DTC.Rental _ -> "Rental"
            DTC.InterCity _ _ -> "InterCity"
            _ -> ""
          driverPartnerName = case merchantShortId.getShortId of
            "BRIDGE_CABS_PARTNER" -> "Bridge Cabs"
            "JATRI_SAATHI_PARTNER" -> "Jatri Saathi"
            "YATRI_PARTNER" -> "Yatri"
            _ -> "Namma Yatri"
      let formattedTitle = T.replace "{#isRentalOrIntercity#}" isRentalOrIntercity title
          fullAddress = fromMaybe "" booking.fromLocation.address.fullAddress
          formattedBody = T.replace "{#pickupAddress#}" fullAddress $ T.replace "{#isRentalOrIntercity#}" isRentalOrIntercity $ T.replace "{#driverPartnerName#}" driverPartnerName body
      (formattedTitle, formattedBody)

    buildCallStatus callStatusId exotelResponse booking = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            entityId = Just booking.id.getId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just booking.providerId.getId,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            callService = Just Exotel,
            callAttempt = Just SCS.Resolved,
            callError = Nothing,
            createdAt = now
          }
