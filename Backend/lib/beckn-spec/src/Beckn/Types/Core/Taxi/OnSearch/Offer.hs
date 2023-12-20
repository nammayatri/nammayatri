{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch.Offer where

import Beckn.Types.Core.Taxi.Common.Descriptor
import Beckn.Types.Core.Taxi.Common.Tags
import qualified Beckn.Types.Core.Taxi.OnSearch.Descriptor as OSD
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data OfferV2 = OfferV2
  { id :: Text,
    descriptor :: Descriptor,
    -- locationIds :: Maybe Text,
    -- categoryIds :: Maybe Text,
    -- itemIds :: Maybe Text,
    -- time :: Maybe Time,
    tags :: [TagGroupV2]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OfferV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Offer = Offer
  { id :: Text,
    descriptor :: OSD.Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Offer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
