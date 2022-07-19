module Beckn.Types.Core.Taxi.Common.Address where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data Address = Address
  { area :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    door :: Maybe Text,
    street :: Maybe Text,
    city :: Maybe Text,
    area_code :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
