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
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantMessage as DMM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import Tools.Error

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"

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
