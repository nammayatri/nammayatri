module API.Beckn.OnStatus.Handler where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnStatus as BecknACL
import Core.Context (validateContext)
import Core.Spec.API.OnStatus
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.OnStatus as OnStatus
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Environment
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnStatusAPI
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  BecknCallbackReq OnStatus.OnStatusMessage ->
  FlowHandler AckResponse
onStatus (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_STATUS $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case req.contents of
    Right msg -> do
      let domainReq = BecknACL.mkOnStatus msg transactionId
      DOnStatus.handler domainReq
      Esq.runTransaction $
        QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    Left err -> logTagError "on_status req" $ "on_status error: " <> show err
  pure Ack
