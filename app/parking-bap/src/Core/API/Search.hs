module Core.API.Search where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Utils.Example
import Core.API.Types (BecknReq)
import Core.Search.Intent (Intent)
import Servant (JSON, Post, ReqBody, (:>))

type SearchAPI =
  "search"
    :> ReqBody '[JSON] (BecknReq SearchIntent)
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Example SearchIntent where
  example = SearchIntent example
