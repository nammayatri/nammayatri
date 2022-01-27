module API.Beckn.OnSearch.Types where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Servant.SignatureAuth
import Core.OnSearch.Catalog
import Servant (JSON, Post, ReqBody, (:>))

type API =
  SignatureAuth "Proxy-Authorization"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
