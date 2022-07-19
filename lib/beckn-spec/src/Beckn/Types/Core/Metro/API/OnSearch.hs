module Beckn.Types.Core.Metro.API.OnSearch where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Metro.OnSearch.Catalog (Catalog)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchReq = BecknCallbackReq OnSearchCatalog

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
