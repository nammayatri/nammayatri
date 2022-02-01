module API.Beckn.OnSearch.Handler where

import App.Types
import Beckn.Prelude
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.ACL.Handler.OnSearch as OnSearchHandler
import qualified Core.ACL.Types.API.OnSearch as OnSearch

publicTransportOnSearch ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  BecknCallbackReq OnSearch.OnSearchCatalog ->
  FlowHandler AckResponse
publicTransportOnSearch _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  OnSearchHandler.publicTransportOnSearch req
