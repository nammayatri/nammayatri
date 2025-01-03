{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnCancel (API, handler) where

import qualified Beckn.ACL.OnCancel as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnCancel as OnCancel
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import Tools.Error
import TransactionLogs.PushLogs

type API = OnCancel.OnCancelAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  OnCancel.OnCancelReqV2 ->
  FlowHandler AckResponse
onCancel _ req = withFlowHandlerBecknAPI do
  transactionId <- Utils.getTransactionId req.onCancelReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "onCancel API Flow" $ "Received onCancel request:-" <> show req
    cancelMsg <- req.onCancelReqMessage & fromMaybeM (InvalidBecknSchema "Missing message in on_cancel")
    cancelStatus' <- cancelMsg.confirmReqMessageOrder.orderStatus & fromMaybeM (InvalidBecknSchema "Missing order.status in on_cancel message")
    logDebug $ "cancelStatus in bpp " <> cancelStatus'
    cancelStatus <- readMaybe (T.unpack cancelStatus') & fromMaybeM (InvalidBecknSchema $ "Invalid order.status:-" <> cancelStatus')
    case cancelStatus of
      Enums.CANCELLED -> processCancellationRequest DOnCancel.onCancel
      Enums.SOFT_CANCEL -> processCancellationRequest DOnCancel.onSoftCancel
      _ -> throwError . InvalidBecknSchema $ "on_cancel order.status expected:-CANCELLED|SOFT_CANCEL, received:-" <> cancelStatus'
  pure Ack
  where
    processCancellationRequest ::
      (DOnCancel.ValidatedOnCancelReq -> Flow ()) ->
      Flow ()
    processCancellationRequest domainOnCancelAction = do
      mbDOnCancelReq <- ACL.buildOnCancelReq req
      messageId <- Utils.getMessageIdText req.onCancelReqContext
      whenJust mbDOnCancelReq $ \onCancelReq ->
        Redis.whenWithLockRedis (onCancelLockKey messageId) 60 $ do
          validatedOnCancelReq <- DOnCancel.validateRequest onCancelReq
          fork "on cancel processing" $ do
            Redis.whenWithLockRedis (onCancelProcessingLockKey messageId) 60 $ do
              domainOnCancelAction validatedOnCancelReq
              fork "on cancel received pushing ondc logs" do
                void $ pushLogs "on_cancel" (toJSON req) validatedOnCancelReq.booking.merchantId.getId "MOBILITY"

onCancelLockKey :: Text -> Text
onCancelLockKey id = "Customer:OnCancel:MessageId-" <> id

onCancelProcessingLockKey :: Text -> Text
onCancelProcessingLockKey id = "Customer:OnCancel:Processing:MessageId-" <> id
