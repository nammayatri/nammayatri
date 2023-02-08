module Core.Spec.API.OnSearch where

import Core.Spec.OnSearch
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Servant.SignatureAuth
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchAPI =
  SignatureAuth "X-Gateway-Authorization"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse
