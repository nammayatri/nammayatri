{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Spec.OnSearch.LocationDetails where

import Beckn.Spec.OnSearch.Descriptor
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
