module Beckn.Types.Core.Taxi.OnSearch.ProviderLocation
  ( module Beckn.Types.Core.Taxi.OnSearch.ProviderLocation,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data ProviderLocation = ProviderLocation
  { id :: Text,
    gps :: Gps
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema ProviderLocation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
