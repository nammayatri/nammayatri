{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Init.Fulfillment
  ( module Beckn.Types.Core.Taxi.Init.Fulfillment,
  )
where

import Beckn.Types.Core.Taxi.Common.FulfillmentType
import Beckn.Types.Core.Taxi.Common.StartInfo
import Beckn.Types.Core.Taxi.Common.StopInfo
import Beckn.Types.Core.Taxi.Common.Vehicle
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common (HighPrecMeters)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Tags = Tags
  { max_estimated_distance :: Maybe HighPrecMeters
  }
  deriving (Generic, Show)

instance ToJSON Tags where
  toJSON = genericToJSON tagsJSONOptions

instance FromJSON Tags where
  parseJSON = genericParseJSON tagsJSONOptions

instance ToSchema Tags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions tagsJSONOptions

tagsJSONOptions :: Options
tagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "max_estimated_distance" -> "./komn/max_estimated_distance"
        a -> a
    }

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulfillmentInfo = FulfillmentInfo
  { id :: Maybe Text,
    _type :: FulfillmentType,
    start :: StartInfo,
    end :: Maybe StopInfo,
    tags :: Tags,
    vehicle :: Vehicle
  }
  deriving (Generic, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
