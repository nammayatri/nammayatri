module Beckn.Types.Core.Metro.API.Search where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Metro.Search.Intent (Intent)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Utils.Example
import EulerHS.Prelude
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
