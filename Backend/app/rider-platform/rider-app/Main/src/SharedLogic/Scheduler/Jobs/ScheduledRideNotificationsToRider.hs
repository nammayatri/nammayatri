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
import qualified Data.Text as T
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.Booking as DB
import Domain.Types.CallStatus
import Domain.Types.RideRelatedNotificationConfig
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Types.Error
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Tools.Call as Call
import Tools.Notifications
import qualified Tools.SMS as Sms

sendScheduledRideNotificationsToRider ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    SchedulerFlow r
  ) =>
  Job 'ScheduledRideNotificationsToRider ->
  m ()
sendScheduledRideNotificationsToRider Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      bookingId = jobData.bookingId
      personId = jobData.personId
      notificationType = jobData.notificationType
      notificationKey = jobData.notificationKey
      bookingStatus = jobData.bookingStatus
  booking <- QB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  ride <- QR.findByRBId bookingId >>= fromMaybeM (RideDoesNotExist bookingId.getId)
  let phoneNumber = countryCode <> mobileNumber

  when (booking.status == bookingStatus) do
    case notificationType of
      CALL -> do
        callStatusId <- generateGUID
        let callReq =
              Call.InitiateCallReq
                { fromPhoneNum = phoneNumber,
                  toPhoneNum = Nothing,
                  attachments = Call.Attachments $ DCall.CallAttachments {callStatusId = callStatusId, rideId = ride.id}
                }
        exotelResponse <- Call.initiateCall booking.merchantId merchantOpCityId callReq
        logTagInfo ("RideId: " <> bookingId.getId) "IVR Call initiated to rider."
        callStatus <- buildCallStatus callStatusId exotelResponse booking ride
        QCallStatus.create callStatus
      PN -> do
        merchantPN <- CPN.findByMerchantOpCityIdAndMessageKey merchantOpCityId notificationKey >>= fromMaybeM (MerchantPNNotFound merchantOpCityId.getId notificationKey)
        let entityData = generateReq merchantPN.title merchantPN.body booking ride
        notifyPersonOnEvents person entityData merchantPN.fcmNotificationType
      SMS -> do
        smsCfg <- asks (.smsCfg)
        let sender = smsCfg.sender
        messageKey <- A.decode (A.encode notificationKey) & fromMaybeM (InvalidRequest "Invalid message key for SMS")
        merchantMessage <- CMM.findByMerchantOperatingCityIdAndMessageKey merchantOpCityId messageKey >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId notificationKey)
        let (_, smsReqBody) = formatMessageTransformer "" merchantMessage.message booking ride
        Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq smsReqBody phoneNumber sender) -- TODO: append SMS heading
          >>= Sms.checkSmsResult
      _ -> pure ()
  where
    generateReq notifTitle notifBody booking ride = do
      let (title, message) = formatMessageTransformer notifTitle notifBody booking ride
      NotifReq
        { title = title,
          message = message
        }

    formatMessageTransformer title body booking ride = do
      let isRentalOrIntercity = case booking.bookingDetails of
            DB.RentalDetails _ -> "Rental "
            DB.InterCityDetails _ -> "InterCity "
            _ -> ""
      let formattedTitle = T.replace "{#isRentalOrIntercity#}" isRentalOrIntercity title
          rideStartOtp = ride.otp
          rideEndOtp = fromMaybe "" ride.endOtp
          formattedBody = T.replace "{#rideStartOtp#}" rideStartOtp $ T.replace "{#rideEndOtp#}" rideEndOtp $ T.replace "{#rideStartTime#}" (show booking.startTime) body
      (formattedTitle, formattedBody)

    buildCallStatus callStatusId exotelResponse booking ride = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = Just ride.id,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = Just booking.merchantId.getId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now
          }
