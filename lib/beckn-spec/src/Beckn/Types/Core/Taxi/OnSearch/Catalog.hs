module Beckn.Types.Core.Taxi.OnSearch.Catalog where

import Beckn.Types.Core.Taxi.OnSearch.Descriptor
import Beckn.Types.Core.Taxi.OnSearch.Provider
import Beckn.Utils.JSON (slashedRecordFields)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import EulerHS.Prelude hiding (exp, id)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: NonEmpty Provider
  }
  deriving (Generic, Show)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
