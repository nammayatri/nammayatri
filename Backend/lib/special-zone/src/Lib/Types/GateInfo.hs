{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Types.GateInfo where

import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Types.SpecialLocation

data GateInfoFull = GateInfoFull
  { id :: Id GateInfo,
    specialLocationId :: Id SpecialLocation,
    defaultDriverExtra :: Maybe Int,
    point :: LatLong,
    name :: Text,
    address :: Maybe Text,
    geoJson :: Maybe Text,
    canQueueUpOnGate :: Bool,
    gateType :: GateType,
    gateTags :: Maybe [Text],
    walkDescription :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data GateType = Pickup | Drop
  deriving (Read, Show, Generic, Eq, FromJSON, ToJSON, ToSchema)

data GateInfo = GateInfo
  { id :: Id GateInfo,
    point :: LatLong,
    specialLocationId :: Id SpecialLocation,
    defaultDriverExtra :: Maybe Int,
    name :: Text,
    address :: Maybe Text,
    geom :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canQueueUpOnGate :: Bool,
    gateType :: GateType,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    gateTags :: Maybe [Text],
    walkDescription :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
