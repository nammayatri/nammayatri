module Types.Storage.CallStatus where

import Beckn.External.Exotel.Types hiding (rideId)
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.Ride

data CallStatusT f = CallStatus
  { id :: B.C f (Id CallStatus),
    exotelCallSid :: B.C f Text,
    rideId :: B.C f (Id Ride),
    status :: B.C f ExotelCallStatus,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type CallStatus = CallStatusT Identity

type CallStatusPrimaryKey = B.PrimaryKey CallStatusT Identity

instance B.Table CallStatusT where
  data PrimaryKey CallStatusT f = CallStatusPrimaryKey (B.C f (Id CallStatus))
    deriving (Generic, B.Beamable)
  primaryKey a = CallStatusPrimaryKey a.id

instance ToJSON CallStatus

instance FromJSON CallStatus

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CallStatusT)
fieldEMod =
  B.setEntityName "call_status"
    <> B.modifyTableFields
      B.tableModification
        { exotelCallSid = "exotel_call_sid",
          rideId = "ride_id",
          createdAt = "created_at"
        }

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
