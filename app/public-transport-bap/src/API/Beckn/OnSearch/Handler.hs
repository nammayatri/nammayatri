module API.Beckn.OnSearch.Handler where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnSearch as BecknACL
import Core.Context (validateContext)
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.OnSearch as OnSearch
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment

handler ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  FlowHandler AckResponse
handler (SignatureAuthResult signPayload _) _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext Context.ON_SEARCH $ req.context
  case req.contents of
    Right msg -> do
      domainReq <- BecknACL.buildOnSearch req msg.catalog
      DOnSearch.handler domainReq
      Esq.runTransaction $
        QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack
