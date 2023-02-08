module API.Beckn.OnSearch.Handler where

import qualified Core.ACL.OnSearch as BecknACL
import Core.Context (validateContext)
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.OnSearch as OnSearch
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
handler _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_SEARCH $ req.context
  case req.contents of
    Right msg -> do
      domainReq <- BecknACL.buildOnSearch req msg.catalog
      DOnSearch.handler domainReq
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack
