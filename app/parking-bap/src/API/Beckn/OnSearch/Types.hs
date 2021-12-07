module API.Beckn.OnSearch.Types where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.API.Types (BecknCallbackReq)
import Core.OnSearch.Catalog (Catalog)
import Servant (JSON, Post, ReqBody, (:>))

type API =
  "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
