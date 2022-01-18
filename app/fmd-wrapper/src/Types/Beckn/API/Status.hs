module Types.Beckn.API.Status (module Types.Beckn.API.Status) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] (BecknReq OrderId)
    :> Post '[JSON] AckResponse

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

newtype OrderId = OrderId
  { order_id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema OrderId where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
