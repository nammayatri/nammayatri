{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
handler _ onConfirmCb = withFlowHandlerAPI' . withTransactionIdLogTag onConfirmCb $ do
  validateContext Context.ON_CONFIRM $ onConfirmCb.context
  transactionId <- onConfirmCb.context.transaction_id & fromMaybeM (InvalidRequest "Context.transaction_id is not present.")
  case onConfirmCb.contents of
    Right msg -> do
      handleOnConfirm $ mkDomainOnConfirm (Id transactionId) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  pure Ack
