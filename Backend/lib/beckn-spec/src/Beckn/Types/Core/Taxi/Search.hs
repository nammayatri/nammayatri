{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search
  ( module Beckn.Types.Core.Taxi.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Descriptor as Reexport
import Beckn.Types.Core.Taxi.Common.Tags as Reexport
import Beckn.Types.Core.Taxi.Search.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Search.Intent as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.Payment as Reexport
import Beckn.Types.Core.Taxi.Search.Stop as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.Utils.JSON

-- import Kernel.Types.Common

-- data RouteInfo = RouteInfo
--   { duration :: Maybe Seconds,
--     distance :: Maybe Meters
--   }
--   deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype SearchMessage = SearchMessage
  { intent :: Intent
  -- routeInfo :: Maybe RouteInfo,
  -- device :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON SearchMessage where
  toJSON = genericToJSON removeNullFields

instance FromJSON SearchMessage where
  parseJSON = genericParseJSON removeNullFields

newtype SearchMessageV2 = SearchMessageV2
  { intent :: IntentV2
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON SearchMessageV2 where
  toJSON = genericToJSON removeNullFields

instance FromJSON SearchMessageV2 where
  parseJSON = genericParseJSON removeNullFields

-- searchMessageJSONOptions :: Options
-- searchMessageJSONOptions =
--   defaultOptions
--     { fieldLabelModifier = \case
--         "routeInfo" -> "./nammayatri/routeInfo"
--         a -> a,
--       omitNothingFields = True
--     }
