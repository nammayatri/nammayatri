module Beckn.Types.Core.Taxi.OnConfirm.Location
  ( module Beckn.Types.Core.Taxi.OnConfirm.Location,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.Address as Reexport
import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data Location = Location
  { gps :: Gps,
    address :: Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
