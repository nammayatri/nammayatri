module Beckn.Types.Core.Migration.API.Search where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Catalog (Catalog)
import Beckn.Types.Core.Migration.Intent (Intent)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import Beckn.Utils.Example
import EulerHS.Prelude
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
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example SearchIntent where
  example = SearchIntent example

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON)
