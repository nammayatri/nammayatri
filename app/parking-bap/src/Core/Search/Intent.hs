module Core.Search.Intent where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Search.Location
import Core.Time (Time)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FulFillmentInfo = FulFillmentInfo
  { start :: TimeInfo,
    end :: LocationAndTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulFillmentInfo where
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
