module Core.Spec.API.Search where

import Core.Spec.Search
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes
import Servant (JSON, Post, ReqBody, (:>))

type SearchAPI =
  "search"
    :> ReqBody '[JSON] (BecknReq SearchMessage)
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy
