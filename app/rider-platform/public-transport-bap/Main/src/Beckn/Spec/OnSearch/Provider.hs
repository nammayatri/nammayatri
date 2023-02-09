module Beckn.Spec.OnSearch.Provider where

import Beckn.Spec.OnSearch.Departure
import Beckn.Spec.OnSearch.Descriptor
import Beckn.Spec.OnSearch.Fare
import Beckn.Spec.OnSearch.Item
import Beckn.Spec.OnSearch.LocationDetails
import Beckn.Spec.OnSearch.Route
import Data.OpenApi hiding (items)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Provider = Provider
  { id :: Text,
    descriptor :: DescriptorId,
    -- categories?
    locations :: [LocationDetails],
    routes :: [Route],
    fares :: [Fare],
    departures :: [Departure],
    items :: [Item]
  }
  deriving (Generic, FromJSON, Show, ToJSON)

instance ToSchema Provider where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
