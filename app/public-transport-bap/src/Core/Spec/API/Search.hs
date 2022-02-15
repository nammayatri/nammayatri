module Core.Spec.API.Search where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Servant (JSON, Post, ReqBody, (:>))
import Types.Domain.Outgoing.Search

type SearchAPI =
  "search"
    :> ReqBody '[JSON] (BecknReq SearchIntent)
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy
