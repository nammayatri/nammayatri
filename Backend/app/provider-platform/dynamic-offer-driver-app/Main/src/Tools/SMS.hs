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
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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

sendSMS :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> SendSMSReq -> m SendSMSRes
sendSMS merchantId merchantOpCityId = Sms.sendSMS handler
  where
    handler = Sms.SmsHandler {..}

    getProvidersPriorityList = do
      merchantConfig <- QMSUC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
      let smsServiceProviders = merchantConfig.smsProvidersPriorityList
      when (null smsServiceProviders) $ throwError $ InternalError ("No sms service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId)
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
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DashboardMessageType ->
  Maybe DR.Ride ->
  Id DP.Person ->
  Maybe SRB.Booking ->
  HighPrecMoney ->
  m ()
sendDashboardSms merchantId merchantOpCityId messageType mbRide driverId mbBooking amount = do
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
              MessageBuilder.buildBookingMessage merchantOpCityId $
                MessageBuilder.BuildBookingMessageReq
                  { otp = ride.otp,
                    amount = show booking.estimatedFare
                  }
            sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        ENDRIDE -> whenJust mbRide \ride -> do
          message <-
            MessageBuilder.buildEndRideMessage merchantOpCityId $
              MessageBuilder.BuildEndRideMessageReq
                { rideAmount = show amount,
                  rideShortId = ride.shortId.getShortId
                }
          sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        ONBOARDING -> do
          message <-
            MessageBuilder.buildOnboardingMessage merchantOpCityId $
              MessageBuilder.BuildOnboardingMessageReq
                {
                }
          sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        CASH_COLLECTED -> do
          message <-
            MessageBuilder.buildCollectCashMessage merchantOpCityId $
              MessageBuilder.BuildCollectCashMessageReq
                { amount = show amount
                }
          sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
    else do
      logInfo "Merchant not configured to send dashboard sms"
