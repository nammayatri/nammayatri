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
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Overlay as DOverlay
import Domain.Types.Person
import Domain.Types.RideRelatedNotificationConfig
import Domain.Types.TransporterConfig
import Kernel.External.Call.Interface.Types
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (..), SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
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

type ScheduleNotificationFlow m r =
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    SchedulerFlow r
  )

sendScheduledRideNotificationsToDriver ::
  (ScheduleNotificationFlow m r, HasKafkaProducer r) =>
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
  when (isNotificationRequired && booking.status `notElem` [DB.CANCELLED, DB.REALLOCATED]) do
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    let maybeAppId = (HM.lookup RentalAppletID . exotelMap) =<< transporterConfig.exotelAppIdMapping
    sendCommunicationToDriver $ SendCommunicationToDriverReq {entityId = Just booking.id.getId, messageTransformer = formatMessageTransformer booking merchant.shortId, ..}
  return Complete
  where
    formatMessageTransformer :: DB.Booking -> ShortId Merchant -> (Text, Text) -> (Text, Text)
    formatMessageTransformer booking merchantShortId (title, body) = do
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

sendTagActionNotification ::
  (ScheduleNotificationFlow m r, HasKafkaProducer r) =>
  Job 'ScheduleTagActionNotification ->
  m ExecutionResult
sendTagActionNotification Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      driverId = jobData.driverId
      notificationType = jobData.notificationType
      notificationKey = jobData.notificationKey
      merchantId = jobData.merchantId
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  sendCommunicationToDriver $ SendCommunicationToDriverReq {maybeAppId = Nothing, entityId = Nothing, messageTransformer = identity, ..}
  return Complete

data SendCommunicationToDriverReq = SendCommunicationToDriverReq
  { notificationType :: NotificationType,
    notificationKey :: Text,
    maybeAppId :: Maybe Text,
    entityId :: Maybe Text,
    messageTransformer :: (Text, Text) -> (Text, Text),
    merchant :: Merchant,
    merchantOpCityId :: Id MerchantOperatingCity,
    driverId :: Id Person
  }

sendCommunicationToDriver ::
  (ScheduleNotificationFlow m r, HasKafkaProducer r) =>
  SendCommunicationToDriverReq ->
  m ()
sendCommunicationToDriver SendCommunicationToDriverReq {..} = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
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
                attachments = Attachments $ CallAttachments {callStatusId, entityId = fromMaybe driverId.getId entityId},
                appletId = maybeAppId
              }
      exotelResponse <- initiateCall merchant.id merchantOpCityId callReq
      callStatus <- buildCallStatus callStatusId exotelResponse
      void $ QCallStatus.create callStatus
    PN -> do
      merchantPN <- CPN.findMatchingMerchantPN merchantOpCityId notificationKey Nothing Nothing driver.language Nothing >>= fromMaybeM (MerchantPNNotFound merchantOpCityId.getId notificationKey)
      let entityData = generateReq merchantPN.title merchantPN.body
      notifyDriverOnEvents merchantOpCityId driverId driver.deviceToken entityData merchantPN.fcmNotificationType
    OVERLAY -> do
      overlayKey <- A.decode (A.encode notificationKey) & fromMaybeM (InvalidRequest "Invalid overlay key for Notification")
      merchantOverlay <- CMO.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory merchantOpCityId overlayKey ENGLISH Nothing Nothing Nothing >>= fromMaybeM (OverlayKeyNotFound notificationKey)
      let (title, description) = messageTransformer ((fromMaybe "" merchantOverlay.title), (fromMaybe "" merchantOverlay.description))
      let overlay :: DOverlay.Overlay = overlay {DOverlay.title = Just title, DOverlay.description = Just description}
      sendOverlay merchantOpCityId driver $ mkOverlayReq overlay
    SMS -> do
      smsCfg <- asks (.smsCfg)
      messageKey <- A.decode (A.encode notificationKey) & fromMaybeM (InvalidRequest "Invalid message key for SMS")
      merchantMessage <- CMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey Nothing Nothing >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId notificationKey)
      let sender = fromMaybe smsCfg.sender merchantMessage.senderHeader
          (_, messageBody) = messageTransformer ("", merchantMessage.message)
      Sms.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq messageBody phoneNumber sender merchantMessage.templateId merchantMessage.messageType) >>= Sms.checkSmsResult
    _ -> pure () -- WHATSAPP or Other Notifications can be implemented here
  where
    generateReq notifTitle notifBody = do
      let (title, message) = messageTransformer (notifTitle, notifBody)
      NotifReq
        { title = title,
          message = message,
          entityId = fromMaybe driverId.getId entityId
        }

    buildCallStatus callStatusId exotelResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            entityId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just merchant.id.getId,
            merchantOperatingCityId = Just merchantOpCityId,
            callService = Just Exotel,
            callAttempt = Just SCS.Resolved,
            callError = Nothing,
            createdAt = now,
            aiCallAnalyzed = Nothing
          }
