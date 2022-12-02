module Core.Spec.OnSearch.Departure where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi

data Departure = Departure
  { id :: Text,
    route_id :: Text,
    start_time :: TimeStamp,
    end_time :: TimeStamp
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema Departure where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype TimeStamp = TimeStamp
  { timestamp :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSchema TimeStamp where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
