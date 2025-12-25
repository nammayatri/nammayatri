{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.RideRoute where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.External.Maps (LatLong)
import Kernel.Utils.Common
import qualified Tools.Maps as Maps

data RouteInfo = RouteInfo
  { duration :: Maybe Seconds,
    distance :: Maybe Meters,
    distanceWithUnit :: Maybe Distance,
    points :: Maybe [LatLong]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DeviationInfo = DeviationInfo
  { deviation :: Bool,
    safetyDeviation :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RouteAndDeviationInfo = RouteAndDeviationInfo
  { routeInfo :: RouteInfo,
    deviationInfo :: DeviationInfo
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

createMultipleRouteInfo :: Maps.RouteInfo -> RouteAndDeviationInfo
createMultipleRouteInfo Maps.RouteInfo {..} =
  RouteAndDeviationInfo
    { routeInfo =
        RouteInfo
          { points = Just points,
            ..
          },
      deviationInfo =
        DeviationInfo {deviation = False, safetyDeviation = False}
    }
