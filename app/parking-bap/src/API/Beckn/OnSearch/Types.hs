module API.Beckn.OnSearch.Types where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Servant.SignatureAuth
import Core.OnSearch.Catalog (Catalog)
import Servant

type API =
  SignatureAuth "X-Gateway-Authorization"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
