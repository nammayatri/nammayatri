module API.Beckn.OnSearch.Types where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Servant.SignatureAuth
import Core.ACL.Types.API.OnSearch
import Servant (JSON, Post, ReqBody, (:>))

type API =
  SignatureAuth "X-Gateway-Provider"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse
