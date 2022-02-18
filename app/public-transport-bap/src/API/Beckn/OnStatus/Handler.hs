module API.Beckn.OnStatus.Handler where

import App.Types
import Beckn.Prelude
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.ACL.OnStatus as BecknACL
import Core.Spec.API.OnStatus
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.OnStatus as OnStatus
import qualified Domain.Endpoints.Beckn.OnStatus as DOnStatus
import Tools.Context (validateContext)

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
  case req.contents of
    Right msg -> do
      let domainReq = BecknACL.mkOnStatus msg req.context.transaction_id
      DOnStatus.handler domainReq
    Left err -> logTagError "on_status req" $ "on_status error: " <> show err
  pure Ack
