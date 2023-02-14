 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Location where

import Beckn.Types.Core.Metro.Search.Address (Address)
import Beckn.Types.Core.Metro.Search.Circle (Circle)
import Beckn.Types.Core.Metro.Search.City (City)
import Beckn.Types.Core.Metro.Search.Country (Country)
import Beckn.Types.Core.Metro.Search.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.Search.Gps (Gps)
import Beckn.Types.Core.Metro.Search.Time (Time)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Example
import Kernel.Utils.JSON

data Location = Location
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    gps :: Maybe Gps,
    address :: Maybe Address,
    station_code :: Maybe Text,
    city :: Maybe City,
    country :: Maybe Country,
    circle :: Maybe Circle,
    polygon :: Maybe Text,
    _3dspace :: Maybe Text,
    time :: Maybe Time
  }
  deriving (Generic, Show, ToSchema)

emptyLocation :: Location
emptyLocation =
  Location
    { id = Nothing,
      descriptor = Nothing,
      gps = Nothing,
      address = Nothing,
      station_code = Nothing,
      city = Nothing,
      country = Nothing,
      circle = Nothing,
      polygon = Nothing,
      _3dspace = Nothing,
      time = Nothing
    }

instance Example Location where
  example =
    Location
      { id = Nothing,
        descriptor = Nothing,
        gps = Nothing,
        address = Nothing,
        station_code = Nothing,
        city = Nothing,
        country = Nothing,
        circle = Nothing,
        polygon = Nothing,
        _3dspace = Nothing,
        time = Nothing
      }

instance FromJSON Location where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Location where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
