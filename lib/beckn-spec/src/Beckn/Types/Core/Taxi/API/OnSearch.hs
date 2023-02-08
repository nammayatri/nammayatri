module Beckn.Types.Core.Taxi.API.OnSearch where

import Beckn.Types.Core.Taxi.OnSearch
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchReq = BecknCallbackReq OnSearchMessage

type OnSearchRes = AckResponse

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
