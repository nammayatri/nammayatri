module Beckn.Types.Core.Taxi.Track.Order
  ( module Beckn.Types.Core.Taxi.Track.Order,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Order = Order
  { id :: Text --bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
