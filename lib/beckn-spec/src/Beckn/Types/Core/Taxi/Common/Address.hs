module Beckn.Types.Core.Taxi.Common.Address where

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Address = Address
  { area :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    city :: Maybe Text,
    area_code :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
