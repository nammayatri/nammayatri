module Beckn.Types.Core.Cabs.API.OnSearch where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Cabs.API.Types (BecknCallbackReq)
import Beckn.Types.Core.Cabs.OnSearch
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchReq = BecknCallbackReq OnSearchMessage

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
