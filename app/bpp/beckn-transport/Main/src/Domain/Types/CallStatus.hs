module Domain.Types.CallStatus where

import Domain.Types.Ride
import Kernel.External.Exotel.Types hiding (rideId)
import Kernel.Prelude
import Kernel.Types.Id

data CallStatus = CallStatus
  { id :: Id CallStatus,
    exotelCallSid :: Text,
    rideId :: Id Ride,
    status :: ExotelCallStatus,
    recordingUrl :: Maybe Text,
    conversationDuration :: Int,
    createdAt :: UTCTime
  }
  deriving (Generic)

data CallStatusAPIEntity = CallStatusAPIEntity
  { callId :: Id CallStatus,
    rideId :: Id Ride,
    status :: ExotelCallStatus
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

makeCallStatusAPIEntity :: CallStatus -> CallStatusAPIEntity
makeCallStatusAPIEntity CallStatus {..} =
  CallStatusAPIEntity
    { callId = id,
      ..
    }
