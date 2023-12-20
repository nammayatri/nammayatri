{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch.Category
  ( module Beckn.Types.Core.Taxi.OnSearch.Category,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Descriptor
import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.Common.Tags
import qualified Beckn.Types.Core.Taxi.OnSearch.Descriptor as OSD
-- import Beckn.Types.Core.Taxi.Common.Time
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
-- import Kernel.Types.Common (Seconds)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data CategoryV2 = CategoryV2
  { id :: FareProductType,
    descriptor :: DescriptorV2,
    -- parentCategoryId :: Maybe Text,
    -- time :: Maybe Time,
    -- ttl :: Maybe Seconds,
    tags :: Maybe [TagGroupV2]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema CategoryV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Category = Category
  { id :: FareProductType,
    descriptor :: OSD.Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Category where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
