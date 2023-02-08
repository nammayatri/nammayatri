module Beckn.Types.Core.Taxi.OnSelect.Location
  ( module Beckn.Types.Core.Taxi.OnSelect.Location,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Location = Location
  { gps :: Gps
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
