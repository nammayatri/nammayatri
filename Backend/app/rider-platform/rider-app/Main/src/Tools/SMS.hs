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
    sendBookingOTPMessage,
    DashboardMessageType,
  )
where

import qualified Domain.Types.Booking as DRB
import Domain.Types.Booking.Type (BookingDetails (OneWaySpecialZoneDetails))
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.External.SMS as Reexport hiding
  ( sendSMS,
  )
import qualified Kernel.External.SMS as Sms
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.CachedQueries.CacheConfig (CacheFlow, HasCacheConfig)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Metrics

sendSMS :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id Merchant -> SendSMSReq -> m SendSMSRes
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

data DashboardMessageType = BOOKING_OTP deriving (Show, Generic, Eq)

sendBookingOTPMessage ::
  ( EsqDBReplicaFlow m r,
    HasCacheConfig r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id Merchant ->
  Id DP.Person ->
  Id DRB.Booking ->
  m ()
sendBookingOTPMessage merchantId personId bookingId = do
  -- merchant access check
  merchantConfig <- QMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  if merchantConfig.enableDashboardSms
    then do
      -- customer <- Esq.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      customer <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      -- booking <- Esq.runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingId.getId)
      booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingId.getId)
      smsCfg <- asks (.smsCfg)
      mobileNumber <- mapM decrypt customer.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      let countryCode = fromMaybe "+91" customer.mobileCountryCode
      let phoneNumber = countryCode <> mobileNumber
          sender = smsCfg.sender
      mbotp <- case booking.bookingDetails of
        OneWaySpecialZoneDetails b -> return b.otpCode
        _ -> pure Nothing
      let amount = booking.estimatedTotalFare
      whenJust mbotp \otp -> do
        message <-
          MessageBuilder.buildSendBookingOTPMessage merchantId $
            MessageBuilder.BuildSendBookingOTPMessageReq
              { otp = show otp,
                amount = show amount
              }
        sendSMS merchantId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        pure ()
    else do
      logInfo "Merchant not configured to send dashboard sms"
