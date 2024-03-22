{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Select (API, handler) where

import qualified Beckn.ACL.Select as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import BecknV2.Utils
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import Tools.TransactionLogs
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Select.SelectAPIV2

handler :: FlowServer API
handler = select

select ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Select.SelectReqV2 ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI $ do
  transactionId <- Utils.getTransactionId reqV2.selectReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "SelectV2 API Flow" "Reached"
    dSelectReq <- ACL.buildSelectReqV2 subscriber reqV2

    Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
      (merchant, estimate) <- DSelect.validateRequest transporterId dSelectReq
      fork "select request processing" $ do
        Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
          DSelect.handler merchant dSelectReq estimate
      fork "select received pushing ondc logs" do
        let kafkaLog = TransactionLog "select" $ Req reqV2.selectReqContext (toJSON reqV2.selectReqMessage)
        pushBecknLogToKafka kafkaLog
        let transactionLog = TransactionLogReq "select" $ ReqLog (toJSON reqV2.selectReqContext) (maskSensitiveData $ toJSON reqV2.selectReqMessage)
        becknConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapVariantToVehicle estimate.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
        void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
    pure Ack

selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id

selectProcessingLockKey :: Text -> Text
selectProcessingLockKey id = "Driver:Select:Processing:MessageId-" <> id
