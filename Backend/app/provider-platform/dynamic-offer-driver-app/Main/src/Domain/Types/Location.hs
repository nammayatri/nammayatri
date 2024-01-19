{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Location where

import Data.Aeson
import Data.Time
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.HasCoordinates (HasCoordinates)
import Kernel.Prelude
import Kernel.Types.Common (Meters (..))
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data Location = Location
  { id :: Id Location,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, PrettyShow, ToJSON, FromJSON, ToSchema)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    fullAddress :: Maybe Text
  }
  deriving (Generic, Show, Eq, PrettyShow, ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    fullAddress :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data DummyLocationInfo = DummyLocationInfo
  { dummyId :: Text,
    lat :: Double,
    lon :: Double,
    distance :: Meters,
    baseFare :: Int,
    door :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    area :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    areaCode :: Maybe Text,
    fullAddress :: Maybe Text
  }
  deriving (Generic, Show, Read, Eq, PrettyShow, ToJSON, FromJSON, ToSchema)

makeLocationAPIEntity :: Location -> LocationAPIEntity
makeLocationAPIEntity Location {..} = do
  let LocationAddress {..} = address
  LocationAPIEntity
    { ..
    }

dummyFromLocationData :: DummyLocationInfo
dummyFromLocationData =
  DummyLocationInfo
    { dummyId = "dummyFromLocation",
      lat = 12.9421783,
      lon = 77.62205,
      distance = Meters 1000,
      baseFare = 40,
      door = Just "817",
      building = Just "20th Main Rd",
      street = Just "Koramangala 8th Block",
      area = Just "Koramangala, Koramangala 8th Block, 20th Main Rd",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      areaCode = Just "560095",
      fullAddress = Just "817, 20th Main Rd, Koramangala 8th Block, Koramangala, Bengaluru, Karnataka 560095, 560095, India"
    }

dummyToLocationData :: DummyLocationInfo
dummyToLocationData =
  DummyLocationInfo
    { dummyId = "dummyToLocation",
      lat = 12.938797,
      lon = 77.624116,
      distance = Meters 1000,
      baseFare = 0,
      door = Just "831",
      building = Just "17th F Main Rd",
      street = Just "6th Block",
      area = Just "Koramangala, 6th Block",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      areaCode = Just "560095",
      fullAddress = Just "Rohit 17th F Main Rd, 6th Block, Koramangala, Bengaluru, Karnataka 560095, 560095, India"
    }
