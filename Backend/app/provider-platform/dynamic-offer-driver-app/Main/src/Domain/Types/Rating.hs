{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Rating where

import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import Domain.Types.Ride (Ride)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data Rating = Rating
  { id :: Id Rating,
    rideId :: Id Ride,
    driverId :: Id Person,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    wasOfferedAssistance :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    isSafe :: Maybe Bool,
    issueId :: Maybe Text
  }
  deriving (Generic, Show, Eq)
