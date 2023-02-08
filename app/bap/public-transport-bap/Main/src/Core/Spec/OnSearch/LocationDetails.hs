module Core.Spec.OnSearch.LocationDetails where

import Core.Spec.OnSearch.Descriptor
import Data.OpenApi hiding (name)
import Kernel.Prelude
import Kernel.Types.Beckn.Gps
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data LocationDetails = LocationDetails
  { id :: Text,
    descriptor :: DescriptorId,
    gps :: Gps,
    stop_code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema LocationDetails where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
