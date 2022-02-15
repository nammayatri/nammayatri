module Core.Spec.Search.Intent where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.Time (Time)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

newtype Intent = Intent
  { fulfillment :: FulFillmentInfo
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FulFillmentInfo = FulFillmentInfo
  { start :: LocationAndTime,
    end :: Location
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulFillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype TimeInfo = TimeInfo
  { time :: Time
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema TimeInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data LocationAndTime = LocationAndTime
  { location :: Location,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema LocationAndTime where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Location = Location
  { gps :: Gps
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
