module API.Beckn.OnSearch (API, handler) where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSearch as TaxiACL
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment
import Servant hiding (throwError)

type API =
  SignatureAuth "X-Gateway-Authorization"
    :> OnSearch.OnSearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
onSearch (SignatureAuthResult _ _ registryUrl) _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSearchReq <- TaxiACL.buildOnSearchReq req
  DOnSearch.onSearch registryUrl req.context.message_id mbDOnSearchReq
  pure Ack
