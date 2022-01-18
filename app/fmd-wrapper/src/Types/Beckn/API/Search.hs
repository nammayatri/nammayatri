module Types.Beckn.API.Search (module Types.Beckn.API.Search, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Gps as Reexport (Gps (..))

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
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema SearchIntent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FulFillmentInfo = FulFillmentInfo
  { start :: LocationInfo,
    end :: LocationInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulFillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype LocationInfo = LocationInfo
  { location :: Location
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema LocationInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Location = Location
  { gps :: Gps
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
