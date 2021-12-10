module Beckn.Types.Core.Taxi.API.Search where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Search (SearchMessage)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SearchReq = BecknReq SearchMessage

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy
