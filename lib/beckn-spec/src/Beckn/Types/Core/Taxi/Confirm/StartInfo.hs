module Beckn.Types.Core.Taxi.Confirm.StartInfo where

import Beckn.Types.Core.Taxi.Confirm.Location (Location)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

newtype StartInfo = StartInfo
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
