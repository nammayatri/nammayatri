{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.CallStatus where

import Domain.Types.Ride
import qualified Kernel.External.Call.Interface.Types as CallTypes
import Kernel.Prelude
import Kernel.Types.Id

data CallStatus = CallStatus
  { id :: Id CallStatus,
    callId :: Text,
    rideId :: Id Ride,
    status :: CallTypes.CallStatus,
    recordingUrl :: Maybe Text,
    conversationDuration :: Int,
    createdAt :: UTCTime
  }
  deriving (Generic)

data CallStatusAPIEntity = CallStatusAPIEntity
  { callStatusId :: Id CallStatus,
    rideId :: Id Ride,
    status :: CallTypes.CallStatus
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

makeCallStatusAPIEntity :: CallStatus -> CallStatusAPIEntity
makeCallStatusAPIEntity CallStatus {..} =
  CallStatusAPIEntity
    { callStatusId = id,
      ..
    }
