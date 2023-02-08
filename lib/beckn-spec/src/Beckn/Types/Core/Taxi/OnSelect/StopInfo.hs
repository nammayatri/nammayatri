module Beckn.Types.Core.Taxi.OnSelect.StopInfo where

import Beckn.Types.Core.Taxi.OnSelect.Location (Location)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- FIXME Reuse from Beckn.Types.Core.Taxi.Search.StopInfo when spec changes will be merged
newtype StopInfo = StopInfo
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StopInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
