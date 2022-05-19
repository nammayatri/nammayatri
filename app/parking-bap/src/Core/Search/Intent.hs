module Core.Search.Intent where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Common.Time (Time)
import Core.Search.Location
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Intent = Intent
  { fulfillment :: FulfillmentInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FulfillmentInfo = FulfillmentInfo
  { start :: TimeInfo,
    end :: LocationAndTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype TimeInfo = TimeInfo
  { time :: Time
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema TimeInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data LocationAndTime = LocationAndTime
  { location :: Location,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema LocationAndTime where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
