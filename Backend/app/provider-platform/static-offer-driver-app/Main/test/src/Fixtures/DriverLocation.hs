{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Fixtures.DriverLocation where

import qualified Domain.Types.DriverLocation as DDrLoc
import qualified Fixtures.Time as Fixtures
import Kernel.Types.Id

defaultDriverLocation :: DDrLoc.DriverLocation
defaultDriverLocation =
  DDrLoc.DriverLocation
    { driverId = Id "1",
      lat = 10,
      lon = 10,
      coordinatesCalculatedAt = Fixtures.defaultTime,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
