module Beckn.Types.Core.Taxi.Search.Time where

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Example
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

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
