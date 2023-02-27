{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
