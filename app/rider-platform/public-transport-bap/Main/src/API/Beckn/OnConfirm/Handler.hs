module API.Beckn.OnConfirm.Handler where

import Beckn.ACL.OnConfirm
import Beckn.Context
import Beckn.Spec.API.OnConfirm
import qualified Beckn.Spec.Common.Context as Context
import Domain.Action.Beckn.OnConfirm
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Tools.Error

handler :: SignatureAuthResult -> FlowServer OnConfirmAPI
handler _ onConfirmCb = withFlowHandlerAPI . withTransactionIdLogTag onConfirmCb $ do
  validateContext Context.ON_CONFIRM $ onConfirmCb.context
  transactionId <- onConfirmCb.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case onConfirmCb.contents of
    Right msg -> do
      handleOnConfirm $ mkDomainOnConfirm (Id transactionId) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  pure Ack
