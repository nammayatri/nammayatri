module Domain.Types.CallStatus where

import Beckn.External.Exotel.Types hiding (rideId)
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Domain.Types.Ride
import EulerHS.Prelude hiding (id)

data CallStatus = CallStatus
  { id :: Id CallStatus,
    exotelCallSid :: Text,
    rideId :: Id Ride,
    status :: ExotelCallStatus,
    recordingUrl :: Text,
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
