module API.Beckn.OnConfirm.Handler where

import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Core.Migration.Context as Context
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.ACL.OnConfirm
import Core.Spec.OnConfirm.API
import Domain.Endpoints.OnConfirm
import Tools.Context

handler :: SignatureAuthResult -> FlowServer API
handler _ onConfirmCb = withFlowHandlerAPI . withTransactionIdLogTag onConfirmCb $ do
  validateContext Context.ON_CONFIRM $ onConfirmCb.context
  case onConfirmCb.contents of
    Right msg -> handleOnConfirm $ mkDomainOnConfirm (Id onConfirmCb.context.transaction_id) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  pure Ack
