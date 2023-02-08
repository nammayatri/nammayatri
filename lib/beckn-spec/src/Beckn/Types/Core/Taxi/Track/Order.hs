module Beckn.Types.Core.Taxi.Track.Order
  ( module Beckn.Types.Core.Taxi.Track.Order,
  )
where

import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema

newtype Order = Order
  { id :: Text --bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
