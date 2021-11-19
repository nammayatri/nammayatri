module Beckn.Types.Core.Migration1.API.Search where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration1.API.Types (BecknReq)
import Beckn.Types.Core.Migration1.Search (SearchMessage)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SearchReq = BecknReq SearchMessage

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy
