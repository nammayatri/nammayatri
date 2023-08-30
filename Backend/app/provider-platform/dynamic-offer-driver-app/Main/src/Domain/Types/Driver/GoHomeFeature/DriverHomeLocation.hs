{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Driver.GoHomeFeature.DriverHomeLocation where

import Data.Time
import Domain.Types.Person
import Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import Kernel.Types.Id

data DriverHomeLocation = DriverHomeLocation
  { id :: Id DriverHomeLocation,
    driverId :: Id Person,
    lat :: Double,
    lon :: Double,
    address :: Text,
    tag :: Text,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, HasCoordinates, ToSchema)

data DriverHomeLocationAPIEntity = DriverHomeLocationAPIEntity
  { id :: Id DriverHomeLocation,
    lat :: Double,
    lon :: Double,
    address :: Text,
    tag :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data UpdateDriverHomeLocation = UpdateDriverHomeLocation
  { lat :: Double,
    lon :: Double,
    address :: Text,
    tag :: Text
  }

makeDriverHomeLocationAPIEntity :: DriverHomeLocation -> DriverHomeLocationAPIEntity
makeDriverHomeLocationAPIEntity DriverHomeLocation {..} = DriverHomeLocationAPIEntity {..}
