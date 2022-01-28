module Core.API.OnSearch where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Beckn.Utils.Servant.SignatureAuth
import Core.OnSearch.Catalog (Catalog)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Servant (JSON, Post, ReqBody, (:>))

type OnSearchAPI =
  SignatureAuth "X-Gateway-Authorization"
    :> "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema OnSearchCatalog where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
