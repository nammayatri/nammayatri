{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Types.Ride where

import Kernel.Prelude

data RideStart = RideStart
  { rId :: Text, -- rideId
    st :: Text, -- status
    cAt :: UTCTime -- createdTime
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data RideEnd = RideEnd
  { rId :: Text,
    st :: Text,
    cAt :: UTCTime
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)
