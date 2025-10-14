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
  )
where

import Data.Char (digitToInt)
import qualified Data.Text as T
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.SMS as Reexport hiding
  ( sendSMS,
  )
import qualified Kernel.External.SMS as Sms
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import Tools.Error

sendSMS :: ServiceFlow m r => Id Merchant -> Id DMOC.MerchantOperatingCity -> SendSMSReq -> m SendSMSRes
sendSMS merchantId merchantOperatingCityId req = Sms.sendSMS handler req
  where
    handler = Sms.SmsHandler {..}

    getProvidersPriorityList = do
      merchantConfig <-
        QMSUC.findByMerchantOperatingCityId merchantOperatingCityId
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      let smsServiceProviders = merchantConfig.smsProvidersPriorityList
          phoneNumber = req.phoneNumber
      when (null smsServiceProviders) $
        throwError $
          InternalError
            ( "No sms service provider configured for the merchant, merchantOperatingCityId: "
                <> merchantOperatingCityId.getId
            )
      -- get last digit
      let lastDigit = digitToInt (T.last phoneNumber)
          startIndex = lastDigit `mod` (length smsServiceProviders)

      let shiftedProviders = rotate startIndex smsServiceProviders
      logDebug $ "hihi" <> (T.pack $ show smsServiceProviders) <> (T.pack $ show shiftedProviders)
      pure shiftedProviders

    getProviderConfig provider = do
      merchantSmsServiceConfig <-
        QMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.SmsService provider)
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
      case merchantSmsServiceConfig.serviceConfig of
        DMSC.SmsServiceConfig msc -> pure msc
        _ -> throwError $ InternalError "Unknown Service Config"

rotate :: Int -> [a] -> [a]
rotate n xs = take len . drop (n `mod` len) . cycle $ xs
  where
    len = length xs
