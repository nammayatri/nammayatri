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
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.IOLogging as IOL
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Select.SelectAPIV2

handler :: FlowServer API
handler = select

withForcedDebugLevel ::
  (HasField "loggerEnv" r IOL.LoggerEnv, MonadReader r m) =>
  Text ->
  m a ->
  m a
withForcedDebugLevel tag = local modifyEnv
  where
    modifyEnv env = do
      let logEnv = env.loggerEnv
          updLogEnv = IOL.updateLogLevelAndRawSql (Just DEBUG) logEnv
          updLogEnv' = IOL.appendLogTag tag updLogEnv
      env{loggerEnv = updLogEnv'}

select ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Select.SelectReqV2 ->
  FlowHandler AckResponse
select transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI . withConditionalLogDebug $ do
  transactionId <- Utils.getTransactionId reqV2.selectReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "SelectV2 API Flow" "Reached"
    dSelectReq <- ACL.buildSelectReqV2 subscriber reqV2

    Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
      (merchant, searchRequest, estimates) <- DSelect.validateRequest transporterId dSelectReq
      fork "select request processing" $ do
        Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
          DSelect.handler merchant dSelectReq searchRequest estimates
      fork "select received pushing ondc logs" do
        void $ pushLogs "select" (toJSON reqV2) merchant.id.getId "MOBILITY"
    pure Ack
  where
    withConditionalLogDebug fa =
      if subscriber.subscriber_id == "api.moving.tech/bap/beckn/v1/19dded4d-51dd-4dcd-a07d-d353649d8595"
        then do
          withForcedDebugLevel "ANNA_APP_BAP" fa
        else fa

selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id

selectProcessingLockKey :: Text -> Text
selectProcessingLockKey id = "Driver:Select:Processing:MessageId-" <> id
