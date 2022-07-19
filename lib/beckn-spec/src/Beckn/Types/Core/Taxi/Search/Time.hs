module Beckn.Types.Core.Taxi.Search.Time where

import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)

newtype Time = Time
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Time where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Time where
  example =
    Time
      { timestamp = example
      }
