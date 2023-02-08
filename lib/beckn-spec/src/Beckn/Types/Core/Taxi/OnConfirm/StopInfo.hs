module Beckn.Types.Core.Taxi.OnConfirm.StopInfo where

import Beckn.Types.Core.Taxi.OnConfirm.Location (Location)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype StopInfo = StopInfo
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StopInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
