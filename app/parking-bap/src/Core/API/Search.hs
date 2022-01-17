module Core.API.Search where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Search.Intent (Intent)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
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

instance ToSchema SearchIntent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example SearchIntent where
  example = SearchIntent example
