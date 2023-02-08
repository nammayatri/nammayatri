module Beckn.Types.Core.Taxi.API.Search where

import Beckn.Types.Core.Taxi.Search (SearchMessage)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type SearchReq = BecknReq SearchMessage

type SearchRes = AckResponse

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy
