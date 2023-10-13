{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MessageBuilder
  ( BuildSendOTPMessageReq (..),
    buildSendOTPMessage,
    WelcomeToPlatformMessageReq (..),
    buildWelcomeToPlatformMessage,
    buildSendAlternateNumberOTPMessage,
    BuildEndRideMessageReq (..),
    buildEndRideMessage,
    BuildOnboardingMessageReq (..),
    buildOnboardingMessage,
    BuildBookingMessageReq (..),
    buildBookingMessage,
    BuildCollectCashMessageReq (..),
    buildCollectCashMessage,
    BuildSendPaymentLinkReq (..),
    buildSendPaymentLink,
    BuildGenericMessageReq (..),
    buildGenericMessage,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantMessage as DMM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import Tools.Error

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

data BuildSendPaymentLinkReq = BuildSendPaymentLinkReq
  { paymentLink :: Text,
    amount :: Text
  }
  deriving (Generic)

buildSendPaymentLink :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildSendPaymentLinkReq -> m Text
buildSendPaymentLink merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.SEND_PAYMENT_LINK
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.SEND_PAYMENT_LINK))
  return $
    merchantMessage.message
      & T.replace (templateText "paymentLink") req.paymentLink
      & T.replace (templateText "amount") req.amount

data BuildSendOTPMessageReq = BuildSendOTPMessageReq
  { otp :: Text,
    hash :: Text
  }
  deriving (Generic)

buildSendOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildSendOTPMessageReq -> m Text
buildSendOTPMessage merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.SEND_OTP
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.SEND_OTP))
  return $
    merchantMessage.message
      & T.replace (templateText "otp") req.otp
      & T.replace (templateText "hash") req.hash

newtype WelcomeToPlatformMessageReq = WelcomeToPlatformMessageReq
  { orgName :: Text
  }
  deriving (Generic)

buildWelcomeToPlatformMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> WelcomeToPlatformMessageReq -> m Text
buildWelcomeToPlatformMessage merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.WELCOME_TO_PLATFORM
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.WELCOME_TO_PLATFORM))
  return $
    merchantMessage.message
      & T.replace (templateText "orgName") req.orgName

buildSendAlternateNumberOTPMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildSendOTPMessageReq -> m Text
buildSendAlternateNumberOTPMessage merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.ALTERNATE_NUMBER_OTP
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.ALTERNATE_NUMBER_OTP))
  return $
    merchantMessage.message
      & T.replace (templateText "otp") req.otp
      & T.replace (templateText "hash") req.hash

data BuildEndRideMessageReq = BuildEndRideMessageReq
  { rideAmount :: Text,
    rideShortId :: Text
  }
  deriving (Generic)

buildEndRideMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildEndRideMessageReq -> m Text
buildEndRideMessage merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.END_RIDE_MESSAGE
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.END_RIDE_MESSAGE))
  return $
    merchantMessage.message
      & T.replace (templateText "rideAmount") req.rideAmount
      & T.replace (templateText "rideId") req.rideShortId

data BuildOnboardingMessageReq = BuildOnboardingMessageReq {}
  deriving (Generic)

buildOnboardingMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildOnboardingMessageReq -> m Text
buildOnboardingMessage merchantId _ = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.ONBOARDING_YATRI_MESSAGE
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.ONBOARDING_YATRI_MESSAGE))
  return $
    merchantMessage.message

data BuildBookingMessageReq = BuildBookingMessageReq
  { otp :: Text,
    amount :: Text
  }
  deriving (Generic)

buildBookingMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildBookingMessageReq -> m Text
buildBookingMessage merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.BOOKING_MESSAGE
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.BOOKING_MESSAGE))
  return $
    merchantMessage.message
      & T.replace (templateText "otp") req.otp
      & T.replace (templateText "amount") req.amount

newtype BuildCollectCashMessageReq = BuildCollectCashMessageReq
  { amount :: Text
  }
  deriving (Generic)

buildCollectCashMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> BuildCollectCashMessageReq -> m Text
buildCollectCashMessage merchantId req = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId DMM.CASH_COLLECTED_MESSAGE
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show DMM.CASH_COLLECTED_MESSAGE))
  return $
    merchantMessage.message
      & T.replace (templateText "amount") req.amount

data BuildGenericMessageReq = BuildGenericMessageReq {}
  deriving (Generic)

buildGenericMessage :: (EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> DMM.MessageKey -> BuildGenericMessageReq -> m Text
buildGenericMessage merchantId messageKey _ = do
  merchantMessage <-
    QMM.findByMerchantIdAndMessageKey merchantId messageKey
      >>= fromMaybeM (MerchantMessageNotFound merchantId.getId (show messageKey))
  let jsonData = merchantMessage.jsonData
  return $
    merchantMessage.message
      & T.replace (templateText "var1") (fromMaybe "" jsonData.var1)
      & T.replace (templateText "var2") (fromMaybe "" jsonData.var2)
      & T.replace (templateText "var3") (fromMaybe "" jsonData.var3)
