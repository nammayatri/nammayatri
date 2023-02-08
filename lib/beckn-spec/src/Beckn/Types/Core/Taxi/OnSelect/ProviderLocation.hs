module Beckn.Types.Core.Taxi.OnSelect.ProviderLocation
  ( module Beckn.Types.Core.Taxi.OnSelect.ProviderLocation,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data ProviderLocation = ProviderLocation
  { id :: Text,
    gps :: Gps
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema ProviderLocation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
