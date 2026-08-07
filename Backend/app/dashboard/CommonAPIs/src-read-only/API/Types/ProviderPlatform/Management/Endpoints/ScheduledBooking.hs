{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.ScheduledBooking where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types
import qualified Domain.Types.OpsNote
import qualified Domain.Types.RideStatus
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Kernel.Utils.TH
import Servant
import Servant.Client

data AddOpsNoteReq = AddOpsNoteReq {noteType :: Domain.Types.OpsNote.OpsNoteType, content :: Kernel.Prelude.Maybe Kernel.Prelude.Text, status :: Kernel.Prelude.Maybe Domain.Types.OpsNote.OpsNoteStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AddOpsNoteReq where
  hideSecrets = Kernel.Prelude.identity

data AssignmentStatus
  = ASSIGNED
  | UNASSIGNED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data BookingStatus
  = NEW
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  | REALLOCATED
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  | ByFleetOwner
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data LocationAPIEntity = LocationAPIEntity
  { lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fullAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OpsNoteItem = OpsNoteItem
  { id :: Kernel.Prelude.Text,
    noteType :: Domain.Types.OpsNote.OpsNoteType,
    content :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.OpsNote.OpsNoteStatus,
    bookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdByDashboardUserId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReallocationEventItem = ReallocationEventItem
  { bookingId :: Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideStatus :: Kernel.Prelude.Maybe Domain.Types.RideStatus.RideStatus,
    bookingStatus :: BookingStatus,
    becameCurrentAt :: Kernel.Prelude.UTCTime,
    cancelledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cancellationSource :: Kernel.Prelude.Maybe CancellationSource,
    driverDistToPickupAtCancel :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledBookingInfoRes = ScheduledBookingInfoRes
  { transactionId :: Kernel.Prelude.Text,
    bookingId :: Kernel.Prelude.Text,
    tripCategory :: Domain.Types.TripCategory,
    scheduledAt :: Kernel.Prelude.UTCTime,
    fromLocation :: LocationAPIEntity,
    toLocation :: Kernel.Prelude.Maybe LocationAPIEntity,
    riderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bookingStatus :: BookingStatus,
    rideStatus :: Kernel.Prelude.Maybe Domain.Types.RideStatus.RideStatus,
    driverCurrentLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    driverDistanceToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    driverEtaToPickupSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDurationSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    opsNotes :: [OpsNoteItem],
    reallocationHistory :: [ReallocationEventItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledBookingListItem = ScheduledBookingListItem
  { transactionId :: Kernel.Prelude.Text,
    bookingId :: Kernel.Prelude.Text,
    tripCategory :: Domain.Types.TripCategory,
    scheduledAt :: Kernel.Prelude.UTCTime,
    fromLocation :: LocationAPIEntity,
    toLocation :: Kernel.Prelude.Maybe LocationAPIEntity,
    riderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPhoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bookingStatus :: BookingStatus,
    rideStatus :: Kernel.Prelude.Maybe Domain.Types.RideStatus.RideStatus,
    reallocationCount :: Kernel.Prelude.Int,
    hasPendingOpsFlags :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledBookingListRes = ScheduledBookingListRes {totalItems :: Kernel.Prelude.Int, bookings :: [ScheduledBookingListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateOpsNoteReq = UpdateOpsNoteReq {content :: Kernel.Prelude.Maybe Kernel.Prelude.Text, status :: Kernel.Prelude.Maybe Domain.Types.OpsNote.OpsNoteStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateOpsNoteReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("scheduledBooking" :> (GetScheduledBookingList :<|> GetScheduledBookingInfo :<|> PostScheduledBookingOpsNoteAddHelper :<|> PutScheduledBookingOpsNoteUpdateHelper))

type GetScheduledBookingList =
  ( "list" :> QueryParam "assignmentStatus" AssignmentStatus :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get ('[JSON]) ScheduledBookingListRes
  )

type GetScheduledBookingInfo = (Capture "transactionId" Kernel.Prelude.Text :> "info" :> Get ('[JSON]) ScheduledBookingInfoRes)

type PostScheduledBookingOpsNoteAdd = (Capture "transactionId" Kernel.Prelude.Text :> "opsNote" :> ReqBody ('[JSON]) AddOpsNoteReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostScheduledBookingOpsNoteAddHelper =
  ( Capture "transactionId" Kernel.Prelude.Text :> "opsNote" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           AddOpsNoteReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PutScheduledBookingOpsNoteUpdate = ("opsNote" :> Capture "noteId" Kernel.Prelude.Text :> ReqBody ('[JSON]) UpdateOpsNoteReq :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PutScheduledBookingOpsNoteUpdateHelper =
  ( "opsNote" :> Capture "noteId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) UpdateOpsNoteReq
      :> Put
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data ScheduledBookingAPIs = ScheduledBookingAPIs
  { getScheduledBookingList :: (Kernel.Prelude.Maybe (AssignmentStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ScheduledBookingListRes),
    getScheduledBookingInfo :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient ScheduledBookingInfoRes),
    postScheduledBookingOpsNoteAdd :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> AddOpsNoteReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    putScheduledBookingOpsNoteUpdate :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> UpdateOpsNoteReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkScheduledBookingAPIs :: (Client EulerHS.Types.EulerClient API -> ScheduledBookingAPIs)
mkScheduledBookingAPIs scheduledBookingClient = (ScheduledBookingAPIs {..})
  where
    getScheduledBookingList :<|> getScheduledBookingInfo :<|> postScheduledBookingOpsNoteAdd :<|> putScheduledBookingOpsNoteUpdate = scheduledBookingClient

data ScheduledBookingUserActionType
  = GET_SCHEDULED_BOOKING_LIST
  | GET_SCHEDULED_BOOKING_INFO
  | POST_SCHEDULED_BOOKING_OPS_NOTE_ADD
  | PUT_SCHEDULED_BOOKING_OPS_NOTE_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''AssignmentStatus))

$(Data.Singletons.TH.genSingletons [(''ScheduledBookingUserActionType)])
