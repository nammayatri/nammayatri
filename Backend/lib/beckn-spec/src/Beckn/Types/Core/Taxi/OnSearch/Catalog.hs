{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch.Catalog where

import Beckn.Types.Core.Taxi.Common.Descriptor
import qualified Beckn.Types.Core.Taxi.OnSearch.Descriptor as OSD -- To be removed in next release (using common descriptor)
-- import Beckn.Types.Core.Taxi.Common.Payment
-- import Beckn.Types.Core.Taxi.OnSearch.Fulfillment
-- import Beckn.Types.Core.Taxi.OnSearch.Offer
import Beckn.Types.Core.Taxi.OnSearch.Provider
import Data.OpenApi (ToSchema (..), fromAesonOptions)
-- import Data.Time (UTCTime)
import EulerHS.Prelude hiding (exp, id)
-- import Kernel.Types.Common (Seconds)
import Kernel.Utils.JSON (slashedRecordFields)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data CatalogV2 = CatalogV2
  { descriptor :: DescriptorV2,
    providers :: NonEmpty ProviderV2
    -- payments :: Maybe [Payment],
    -- fulfillments :: [FulfillmentInfo],
    -- offers :: Maybe [Offer],
    -- expiry :: Maybe UTCTime,
    -- ttl :: Maybe Seconds
  }
  deriving (Generic, Show)

instance ToSchema CatalogV2 where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON CatalogV2 where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON CatalogV2 where
  toJSON = genericToJSON slashedRecordFields

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Catalog = Catalog
  { bpp_descriptor :: OSD.Descriptor,
    bpp_providers :: NonEmpty Provider
  }
  deriving (Generic, Show)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
