module Core.Spec.OnSearch.LocationDetails where

import Beckn.Prelude
import Beckn.Types.Core.Gps
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.OnSearch.Descriptor
import Data.OpenApi hiding (name)

data LocationDetails = LocationDetails
  { id :: Text,
    descriptor :: DescriptorId,
    gps :: Gps,
    stop_code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema LocationDetails where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
