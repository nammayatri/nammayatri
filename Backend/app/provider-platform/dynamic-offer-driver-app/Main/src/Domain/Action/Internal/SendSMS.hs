{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SendSMS
  ( sendSMS,
    Shared.SendSMSReq (..),
    Shared.SendSMSRes (..),
  )
where

import qualified Data.Text as T
import Environment
import qualified SMS.Domain as Shared
import qualified SMS.Types as Shared
import Kernel.Prelude
import Kernel.Sms.Config (useFakeSms)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Tools.SMS as Sms

sendSMS ::
  Maybe Text ->
  Text ->
  Context.City ->
  Shared.SendSMSReq ->
  Flow Shared.SendSMSRes
sendSMS apiKey merchantShortIdText city req = do
  merchant <- findMerchantByShortId (ShortId merchantShortIdText)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError (AuthBlocked "Invalid BPP internal api key")
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  smsCfg <- asks (.smsCfg)

  let lookupTemplate key = do
        messageKey <- fromMaybeM (InvalidRequest $ "Invalid message key: " <> key) $ readMaybe (T.unpack key)
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId messageKey Nothing Nothing
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show messageKey))
        pure (merchantMessage.message, fromMaybe smsCfg.sender merchantMessage.senderHeader, merchantMessage.templateId, merchantMessage.messageType)

      generateOtp = generateOTPCode
      useFakeOtp = show <$> useFakeSms smsCfg
      sendSms msg phone sender templateId msgType =
        Sms.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq msg phone sender templateId msgType) >>= Sms.checkSmsResult

  Shared.sendSMS lookupTemplate generateOtp useFakeOtp sendSms req
