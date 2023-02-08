module Core.Spec.OnSearch.Provider where

import Core.Spec.OnSearch.Departure
import Core.Spec.OnSearch.Descriptor
import Core.Spec.OnSearch.Fare
import Core.Spec.OnSearch.Item
import Core.Spec.OnSearch.LocationDetails
import Core.Spec.OnSearch.Route
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
