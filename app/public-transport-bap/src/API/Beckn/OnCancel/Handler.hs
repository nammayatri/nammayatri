module API.Beckn.OnCancel.Handler where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnStatus as BecknACL
import Core.Context (validateContext)
import Core.Spec.API.OnCancel
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.OnCancel as OnCancel
import Core.Spec.OnStatus
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnCancelAPI
handler = onCancel

onCancel ::
  SignatureAuthResult ->
  BecknCallbackReq OnCancel.OnCancelMessage ->
  FlowHandler AckResponse
onCancel (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_CANCEL $ req.context
  case req.contents of
    Right msg -> do
      transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
      let order = msg.order
      let domainReq = BecknACL.mkOnStatus (OnStatusMessage order) transactionId
      logPretty DEBUG "domain request" domainReq
      DOnStatus.handler domainReq
      Esq.runTransaction $
        QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    Left err -> logTagError "on_cancel req" $ "on_cancel error: " <> show err
  pure Ack
