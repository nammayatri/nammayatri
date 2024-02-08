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
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()
import Tools.Error

type API = OnCancel.OnCancelAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  OnCancel.OnCancelReqV2 ->
  FlowHandler AckResponse
onCancel _ req = withFlowHandlerBecknAPI do
  cancelMsg <- req.onCancelReqMessage & fromMaybeM (InvalidRequest "Missing onCancel Message")
  cancelStatus <- cancelMsg.confirmReqMessageOrder.orderStatus & fromMaybeM (InvalidRequest "Missing onCancel orderStatus")
  logDebug $ "cancelStatus in bpp " <> cancelStatus

  when (cancelStatus == show Enums.CANCELLED) do
    (mbDOnCancelReq, messageId) <- do
      transactionId <- Utils.getTransactionId req.onCancelReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        mbDOnCancelReq <- ACL.buildOnCancelReq req
        messageId <- Utils.getMessageIdText req.onCancelReqContext
        pure (mbDOnCancelReq, messageId)
    whenJust mbDOnCancelReq $ \onCancelReq ->
      Redis.whenWithLockRedis (onCancelLockKey messageId) 60 $ do
        validatedOnCancelReq <- DOnCancel.validateRequest onCancelReq
        fork "on cancel processing" $ do
          Redis.whenWithLockRedis (onCancelProcessingLockKey messageId) 60 $
            DOnCancel.onCancel validatedOnCancelReq
  pure Ack

onCancelLockKey :: Text -> Text
onCancelLockKey id = "Customer:OnCancel:MessageId-" <> id

onCancelProcessingLockKey :: Text -> Text
onCancelProcessingLockKey id = "Customer:OnCancel:Processing:MessageId-" <> id
