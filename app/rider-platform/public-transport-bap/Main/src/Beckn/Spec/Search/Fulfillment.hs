module Beckn.Spec.Search.Fulfillment where

import Beckn.Spec.Search.LocationGps
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Kernel.Utils.Schema

data Fulfillment = Fulfillment
  { start :: StartInfo,
    end :: EndInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data StartInfo = StartInfo
  { location :: LocationGps,
    time :: StartTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype StartTime = StartTime {range :: TimeRange}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema StartTime where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data TimeRange = TimeRange
  { start :: UTCTime,
    end :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema TimeRange where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype EndInfo = EndInfo
  { location :: LocationGps
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance ToSchema EndInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
