module API.Beckn.OnConfirm.Handler where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Core.ACL.OnConfirm
import Core.Context
import Core.Spec.API.OnConfirm
import qualified Core.Spec.Common.Context as Context
import Data.Aeson (encode)
import Domain.Action.Beckn.OnConfirm
import Environment
import Tools.Error

handler :: SignatureAuthResult -> FlowServer OnConfirmAPI
handler (SignatureAuthResult signPayload _) onConfirmCb = withFlowHandlerAPI . withTransactionIdLogTag onConfirmCb $ do
  validateContext Context.ON_CONFIRM $ onConfirmCb.context
  transactionId <- onConfirmCb.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case onConfirmCb.contents of
    Right msg -> do
      handleOnConfirm $ mkDomainOnConfirm (Id transactionId) msg
      Esq.runTransaction $
        QBR.logBecknRequest (show $ encode onConfirmCb) (show $ signPayload.signature)
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  pure Ack
