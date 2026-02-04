{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.ScheduledRideNotificationsToRider where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingStatus as DB
import Domain.Types.CallStatus
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person as DPerson
import Domain.Types.Ride as DRide
import Domain.Types.RideRelatedNotificationConfig
import Domain.Types.RiderConfig
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Tools.Call as Call
import Tools.Error
import Tools.Notifications
import qualified Tools.SMS as Sms

type ScheduleNotificationFlow m r =
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    SchedulerFlow r,
    HasKafkaProducer r
  )

sendScheduledRideNotificationsToRider ::
  ScheduleNotificationFlow m r =>
  Job 'ScheduledRideNotificationsToRider ->
  m ExecutionResult
sendScheduledRideNotificationsToRider Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOperatingCityId = jobData.merchantOperatingCityId
      bookingId = jobData.bookingId
      personId = jobData.personId
      notificationType = jobData.notificationType
      notificationKey = jobData.notificationKey
  booking <- QB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  ride <- QR.findByRBId bookingId >>= fromMaybeM (RideDoesNotExist bookingId.getId)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  let maybeAppId = (HM.lookup RentalAppletID . exotelMap) =<< riderConfig.exotelAppIdMapping
  when (booking.status `notElem` [DB.CANCELLED, DB.REALLOCATED]) $
    sendCommunicationToCustomer $
      SendCommunicationToCustomerReq
        { rideId = Just ride.id,
          messageTransformer = formatMessageTransformer booking ride,
          configInExperimentVersions = booking.configInExperimentVersions,
          merchantId = booking.merchantId,
          ..
        }
  return Complete
  where
    formatMessageTransformer booking ride (title, body) = do
      let isRentalOrIntercity = case booking.bookingDetails of
            DB.RentalDetails _ -> "Rental"
            DB.InterCityDetails _ -> "InterCity"
            _ -> ""
      let formattedTitle = T.replace "{#isRentalOrIntercity#}" isRentalOrIntercity title
          rideStartOtp = ride.otp
          rideEndOtp = fromMaybe "" ride.endOtp
          rideStartTime = showTimeIst booking.startTime
          formattedBody = T.replace "{#rideStartOtp#}" rideStartOtp $ T.replace "{#rideEndOtp#}" rideEndOtp $ T.replace "{#isRentalOrIntercity#}" isRentalOrIntercity $ T.replace "{#rideStartTime#}" rideStartTime body
      (formattedTitle, formattedBody)

sendTagActionNotification ::
  ScheduleNotificationFlow m r =>
  Job 'ScheduleTagActionNotification ->
  m ExecutionResult
sendTagActionNotification Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOperatingCityId = jobData.merchantOperatingCityId
      personId = jobData.personId
      notificationType = jobData.notificationType
      notificationKey = jobData.notificationKey
      merchantId = jobData.merchantId
  sendCommunicationToCustomer $ SendCommunicationToCustomerReq {maybeAppId = Nothing, rideId = Nothing, messageTransformer = identity, configInExperimentVersions = [], ..}
  return Complete

data SendCommunicationToCustomerReq = SendCommunicationToCustomerReq
  { notificationType :: NotificationType,
    notificationKey :: Text,
    maybeAppId :: Maybe Text,
    rideId :: Maybe (Id DRide.Ride),
    messageTransformer :: (Text, Text) -> (Text, Text),
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    configInExperimentVersions :: [LYT.ConfigVersionMap],
    personId :: Id DPerson.Person
  }

sendCommunicationToCustomer ::
  (ScheduleNotificationFlow m r, HasKafkaProducer r) =>
  SendCommunicationToCustomerReq ->
  m ()
sendCommunicationToCustomer SendCommunicationToCustomerReq {..} = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
  case notificationType of
    CALL -> do
      callStatusId <- generateGUID
      let callReq =
            Call.InitiateCallReq
              { fromPhoneNum = phoneNumber,
                toPhoneNum = Nothing,
                attachments = Call.Attachments $ DCall.CallAttachments {callStatusId = callStatusId, rideId = fromMaybe (cast person.id) rideId}, -- temporary fix
                appletId = maybeAppId
              }
      exotelResponse <- Call.initiateCall merchantId merchantOperatingCityId callReq
      whenJust rideId $ \id -> logTagInfo ("RideId: " <> id.getId) "IVR Call initiated to rider."
      callStatus <- buildCallStatus callStatusId exotelResponse
      QCallStatus.create callStatus
    PN -> do
      merchantPN <- CPN.findMatchingMerchantPNInRideFlow merchantOperatingCityId notificationKey Nothing Nothing person.language configInExperimentVersions >>= fromMaybeM (MerchantPNNotFound merchantOperatingCityId.getId notificationKey)
      let entityData = generateReq merchantPN.title merchantPN.body
      notifyPersonOnEvents person entityData merchantPN.fcmNotificationType
    SMS -> do
      smsCfg <- asks (.smsCfg)
      messageKey <- A.decode (A.encode notificationKey) & fromMaybeM (InvalidRequest "Invalid message key for SMS")
      merchantMessage <- CMM.findByMerchantOperatingCityIdAndMessageKeyInRideFlow merchantOperatingCityId messageKey configInExperimentVersions >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCityId.getId notificationKey)
      let sender = fromMaybe smsCfg.sender merchantMessage.senderHeader
      let (_, smsReqBody) = messageTransformer ("", merchantMessage.message)
      Sms.sendSMS person.merchantId merchantOperatingCityId (Sms.SendSMSReq smsReqBody phoneNumber sender merchantMessage.templateId) -- TODO: append SMS heading
        >>= Sms.checkSmsResult
    _ -> pure ()
  where
    generateReq notifTitle notifBody = do
      let (title, message) = messageTransformer (notifTitle, notifBody)
      NotifReq
        { title = title,
          message = message
        }

    buildCallStatus callStatusId exotelResponse = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            callAttempt = Nothing,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just merchantId.getId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }
