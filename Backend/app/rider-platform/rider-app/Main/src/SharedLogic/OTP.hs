{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.OTP
  ( OTPChannel (..),
    sendOTP,
    defaultOTPChannel,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as MOC
import qualified Domain.Types.Person as Person
import qualified Email.AWS.Flow as Email
import qualified Email.Types as EmailTypes
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Sms.Config
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Tools.Error
import qualified Tools.SMS as Sms
import qualified Tools.Whatsapp as Whatsapp

data OTPChannel = SMS | WHATSAPP | EMAIL
  deriving (Generic, Show, Enum, Eq, FromJSON, ToJSON, ToSchema)

defaultOTPChannel :: OTPChannel
defaultOTPChannel = SMS

sendOTP ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  OTPChannel ->
  Text ->
  Id Person.Person ->
  Id Merchant.Merchant ->
  Id MOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe EmailTypes.EmailOTPConfig ->
  Maybe Text ->
  m ()
sendOTP otpChannel otpCode personId merchantId merchantOperatingCityId mbCountryCode mbMobileNumber mbEmail mbEmailOTPConfig mbSenderHash = do
  smsCfg <- asks (.smsCfg)
  let otpHash = fromMaybe smsCfg.credConfig.otpHash mbSenderHash

  case otpChannel of
    SMS -> do
      countryCode <- mbCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for SMS OTP channel")
      mobileNumber <- mbMobileNumber & fromMaybeM (InvalidRequest "MobileNumber is required for SMS OTP channel")
      let phoneNumber = countryCode <> mobileNumber
      withLogTag ("personId_" <> getId personId) $ do
        buildSmsReq <-
          MessageBuilder.buildSendOTPMessage merchantOperatingCityId $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        Sms.sendSMS merchantId merchantOperatingCityId (buildSmsReq phoneNumber)
          >>= Sms.checkSmsResult
    WHATSAPP -> do
      countryCode <- mbCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for WHATSAPP OTP channel")
      mobileNumber <- mbMobileNumber & fromMaybeM (InvalidRequest "MobileNumber is required for WHATSAPP OTP channel")
      let phoneNumber = countryCode <> mobileNumber
      withLogTag ("personId_" <> getId personId) $ do
        void $ Whatsapp.whatsAppOptAPI merchantId merchantOperatingCityId (Whatsapp.OptApiReq {phoneNumber = phoneNumber, method = Whatsapp.OPT_IN})
        result <- Whatsapp.whatsAppOtpApi merchantId merchantOperatingCityId (Whatsapp.SendOtpApiReq phoneNumber otpCode)
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp OTP message")
    EMAIL -> do
      receiverEmail <- mbEmail & fromMaybeM (InvalidRequest "Email is required for EMAIL OTP channel")
      emailOTPConfig <- mbEmailOTPConfig & fromMaybeM (RiderConfigNotFound $ "Email OTP config not found for merchantOperatingCityId:- " <> getId merchantOperatingCityId)
      withLogTag ("personId_" <> getId personId) $ do
        L.runIO $ Email.sendEmail emailOTPConfig [receiverEmail] otpCode
