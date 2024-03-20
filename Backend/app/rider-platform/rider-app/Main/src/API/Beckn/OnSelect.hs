{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSelect (API, handler) where

import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import Data.Text as T
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import qualified Domain.Types.VehicleVariant as VehVar
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import Tools.TransactionLogs
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

type API = OnSelect.OnSelectAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReqV2 ->
  FlowHandler AckResponse
onSelect _ reqV2 = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId reqV2.onSelectReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    mbDOnSelectReq <- ACL.buildOnSelectReqV2 reqV2
    messageId <- Utils.getMessageIdText reqV2.onSelectReqContext
    whenJust mbDOnSelectReq $ \onSelectReq ->
      Redis.whenWithLockRedis (onSelectLockKey messageId) 60 $ do
        validatedOnSelectReq <- DOnSelect.validateRequest onSelectReq
        fork "on select received pushing ondc logs" do
          let kafkaLog = TransactionLog "on_select" $ Req reqV2.onSelectReqContext (toJSON reqV2.onSelectReqMessage)
          pushBecknLogToKafka kafkaLog
          let transactionLog = TransactionLogReq "on_select" $ ReqLog (toJSON reqV2.onSelectReqContext) (maskSensitiveData $ toJSON reqV2.onSelectReqMessage)
              variant = fromMaybe VehVar.AUTO_RICKSHAW (listToMaybe onSelectReq.quotesInfo >>= \q -> Just q.vehicleVariant)
          becknConfig <- QBC.findByMerchantIdDomainAndVehicle validatedOnSelectReq.searchRequest.merchantId "MOBILITY" (UCommon.mapVariantToVehicle variant) >>= fromMaybeM (InternalError "Beckn Config not found")
          void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
        fork "on select processing" $ do
          Redis.whenWithLockRedis (onSelectProcessingLockKey messageId) 60 $
            DOnSelect.onSelect validatedOnSelectReq
    pure Ack

onSelectLockKey :: Text -> Text
onSelectLockKey id = "Customer:OnSelect:MessageId-" <> id

onSelectProcessingLockKey :: Text -> Text
onSelectProcessingLockKey id = "Customer:OnSelect:Processing:MessageId-" <> id
