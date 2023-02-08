module Beckn.Types.Core.Metro.API.Search where

import Beckn.Types.Core.Metro.Search.Intent (Intent)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Kernel.Utils.Example
import Servant (JSON, Post, ReqBody, (:>))

type SearchReq = BecknReq SearchIntent

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example SearchIntent where
  example = SearchIntent example
