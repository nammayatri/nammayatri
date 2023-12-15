{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnCancel.Handler where

import qualified Beckn.ACL.OnStatus as BecknACL
import Beckn.Context (validateContext)
import Beckn.Spec.API.OnCancel
import qualified Beckn.Spec.Common.Context as Context
import qualified Beckn.Spec.OnCancel as OnCancel
import Beckn.Spec.OnStatus
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnCancelAPI
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  BecknCallbackReq OnCancel.OnCancelMessage ->
  FlowHandler AckResponse
onCancel _ req = withFlowHandlerBecknAPI' . withTransactionIdLogTag req $ do
  validateContext Context.ON_CANCEL $ req.context
  case req.contents of
    Right msg -> do
      transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
      let order = msg.order
      let domainReq = BecknACL.mkOnStatus (OnStatusMessage order) transactionId
      logPretty DEBUG "domain request" domainReq
      DOnStatus.handler domainReq
    Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
  pure Ack
