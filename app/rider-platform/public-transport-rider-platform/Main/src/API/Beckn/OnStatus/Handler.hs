module API.Beckn.OnStatus.Handler where

import qualified Beckn.ACL.OnStatus as BecknACL
import Beckn.Context (validateContext)
import Beckn.Spec.API.OnStatus
import qualified Beckn.Spec.Common.Context as Context
import qualified Beckn.Spec.OnStatus as OnStatus
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
  FlowServer OnStatusAPI
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  BecknCallbackReq OnStatus.OnStatusMessage ->
  FlowHandler AckResponse
onStatus _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_STATUS $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case req.contents of
    Right msg -> do
      let domainReq = BecknACL.mkOnStatus msg transactionId
      DOnStatus.handler domainReq
    Left err -> logTagError "on_status req" $ "on_status error: " <> show err
  pure Ack
