module Beckn.Types.Core.Taxi.Search.StartInfo where

import Beckn.Types.Core.Taxi.Search.Location (Location)
import Beckn.Types.Core.Taxi.Search.Time (Time)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

data StartInfo = StartInfo
  { location :: Location,
    time :: Time
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example StartInfo where
  example =
    StartInfo
      { location = example,
        time = example
      }
