module Beckn.Types.Core.Taxi.Search.StopInfo where

import Beckn.Types.Core.Taxi.Search.Location (Location)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

newtype StopInfo = StopInfo
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StopInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example StopInfo where
  example =
    StopInfo
      { location = example
      }
