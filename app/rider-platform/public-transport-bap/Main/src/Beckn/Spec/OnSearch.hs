module Beckn.Spec.OnSearch (module Beckn.Spec.OnSearch, module Reexport) where

import Beckn.Spec.OnSearch.Departure as Reexport
import Beckn.Spec.OnSearch.Descriptor as Reexport
import Beckn.Spec.OnSearch.Fare as Reexport
import Beckn.Spec.OnSearch.Item as Reexport
import Beckn.Spec.OnSearch.LocationDetails as Reexport
import Beckn.Spec.OnSearch.Provider as Reexport
import Beckn.Spec.OnSearch.Route as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions)
import Kernel.Prelude
import Kernel.Utils.JSON (slashedRecordFields)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields
