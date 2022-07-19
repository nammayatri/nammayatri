module Beckn.Types.Core.Taxi.Search.Location
  ( module Beckn.Types.Core.Taxi.Search.Location,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude

newtype Location = Location
  { gps :: Gps
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
