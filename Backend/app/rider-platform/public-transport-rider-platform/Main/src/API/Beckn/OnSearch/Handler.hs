{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSearch.Handler where

import qualified Beckn.ACL.OnSearch as BecknACL
import Beckn.Context (validateContext)
import qualified Beckn.Spec.Common.Context as Context
import qualified Beckn.Spec.OnSearch as OnSearch
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))

handler ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  FlowHandler AckResponse
handler _ _ req = withFlowHandlerBecknAPI' . withTransactionIdLogTag req $ do
  validateContext Context.ON_SEARCH $ req.context
  case req.contents of
    Right msg -> do
      domainReq <- BecknACL.buildOnSearch req msg.catalog
      DOnSearch.handler domainReq
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack
