module Core.Spec.OnSearch (module Core.Spec.OnSearch, module Reexport) where

import Beckn.Prelude
import Beckn.Utils.JSON (slashedRecordFields)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.OnSearch.Departure as Reexport
import Core.Spec.OnSearch.Descriptor as Reexport
import Core.Spec.OnSearch.Fare as Reexport
import Core.Spec.OnSearch.Item as Reexport
import Core.Spec.OnSearch.LocationDetails as Reexport
import Core.Spec.OnSearch.Provider as Reexport
import Core.Spec.OnSearch.Route as Reexport
import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions)

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
