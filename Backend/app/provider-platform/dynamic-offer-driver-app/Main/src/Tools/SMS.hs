{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.SMS
  ( module Reexport,
    sendSMS,
    sendDashboardSms,
    DashboardMessageType (..),
  )
where

import qualified Domain.Types.Booking as SRB
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.SMS as Reexport hiding
  ( sendSMS,
  )
import qualified Kernel.External.SMS as Sms
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Person as QPerson
import Tools.Error

sendSMS :: ServiceFlow m r => Id Merchant -> SendSMSReq -> m SendSMSRes
sendSMS merchantId = Sms.sendSMS handler
  where
    handler = Sms.SmsHandler {..}

    getProvidersPriorityList = do
      merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      let smsServiceProviders = merchantConfig.smsProvidersPriorityList
      when (null smsServiceProviders) $ throwError $ InternalError ("No sms service provider configured for the merchant, merchantId:" <> merchantId.getId)
      pure smsServiceProviders

    getProviderConfig provider = do
      merchantSmsServiceConfig <-
        QMSC.findByMerchantIdAndService merchantId (DMSC.SmsService provider)
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      case merchantSmsServiceConfig.serviceConfig of
        DMSC.SmsServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"

data DashboardMessageType = BOOKING | ENDRIDE | ONBOARDING | CASH_COLLECTED deriving (Show, Generic, Eq)

sendDashboardSms ::
  ( EsqDBReplicaFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    ServiceFlow m r
  ) =>
  Id Merchant ->
  DashboardMessageType ->
  Maybe DR.Ride ->
  Id DP.Person ->
  Maybe SRB.Booking ->
  HighPrecMoney ->
  m ()
sendDashboardSms merchantId messageType mbRide driverId mbBooking amount = do
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  if transporterConfig.enableDashboardSms
    then do
      driver <- B.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      -- driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      smsCfg <- asks (.smsCfg)
      mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      let countryCode = fromMaybe "+91" driver.mobileCountryCode
      let phoneNumber = countryCode <> mobileNumber
          sender = smsCfg.sender

      case messageType of
        BOOKING -> whenJust mbRide \ride ->
          whenJust mbBooking \booking -> do
            message <-
              MessageBuilder.buildBookingMessage merchantId $
                MessageBuilder.BuildBookingMessageReq
                  { otp = ride.otp,
                    amount = show booking.estimatedFare
                  }
            sendSMS merchantId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        ENDRIDE -> whenJust mbRide \ride -> do
          message <-
            MessageBuilder.buildEndRideMessage merchantId $
              MessageBuilder.BuildEndRideMessageReq
                { rideAmount = show amount,
                  rideShortId = ride.shortId.getShortId
                }
          sendSMS merchantId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        ONBOARDING -> do
          message <-
            MessageBuilder.buildOnboardingMessage merchantId $
              MessageBuilder.BuildOnboardingMessageReq
                {
                }
          sendSMS merchantId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        CASH_COLLECTED -> do
          message <-
            MessageBuilder.buildCollectCashMessage merchantId $
              MessageBuilder.BuildCollectCashMessageReq
                { amount = show amount
                }
          sendSMS merchantId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
    else do
      logInfo "Merchant not configured to send dashboard sms"
