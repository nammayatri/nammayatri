{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SendSMS where

import Data.Map.Strict (Map, foldlWithKey')
import qualified Data.Text as T
import qualified Domain.Types.MerchantMessage as DMM
import Environment
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Tools.SMS as Sms

data SendSMSReq = SendSMSReq
  { phoneNumber :: Text,
    messageKey :: DMM.MessageKey,
    templateVars :: Map Text Text -- e.g. {"otp": "1234", "validityMinutes": "5"}
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

sendSMS ::
  Maybe Text ->
  Text ->
  Context.City ->
  SendSMSReq ->
  Flow APISuccess
sendSMS apiKey merchantShortIdText city req = do
  merchant <- findMerchantByShortId (ShortId merchantShortIdText)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError (AuthBlocked "Invalid BPP internal api key")
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  smsCfg <- asks (.smsCfg)
  merchantMessage <-
    QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId req.messageKey Nothing Nothing
      >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show req.messageKey))
  let msg = foldlWithKey' (\acc k v -> T.replace ("{#" <> k <> "#}") v acc) merchantMessage.message req.templateVars
      sender = fromMaybe smsCfg.sender merchantMessage.senderHeader
  Sms.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq msg req.phoneNumber sender merchantMessage.templateId merchantMessage.messageType) >>= Sms.checkSmsResult
  pure Success
