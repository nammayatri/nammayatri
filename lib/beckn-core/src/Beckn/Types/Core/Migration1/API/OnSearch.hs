module Beckn.Types.Core.Migration1.API.OnSearch where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration1.API.Types (BecknCallbackReq)
import Beckn.Types.Core.Migration1.OnSearch
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchReq = BecknCallbackReq OnSearchMessage

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
