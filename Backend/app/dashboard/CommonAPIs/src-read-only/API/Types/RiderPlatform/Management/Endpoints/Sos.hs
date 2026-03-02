{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Sos where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

newtype SosDetailsMaybeRes = SosDetailsMaybeRes {details :: Kernel.Prelude.Maybe SosDetailsRes}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosDetailsRes = SosDetailsRes
  { id :: Kernel.Types.Id.Id Dashboard.Common.Sos,
    personId :: Kernel.Types.Id.Id Dashboard.Common.Customer,
    rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride,
    flow :: SosType,
    status :: SosStatus,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mediaFiles :: [Kernel.Prelude.Text],
    trackingExpiresAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    sosState :: Kernel.Prelude.Maybe SosState,
    entityType :: Kernel.Prelude.Maybe SosEntityType,
    externalReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    externalReferenceStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    externalStatusHistory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosEntityType
  = Ride
  | NonRide
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosLocationRes = SosLocationRes {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, accuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosState
  = LiveTracking
  | SosActive
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosStatus
  = NotResolved
  | Pending
  | Resolved
  | MockPending
  | MockResolved
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosTrackingRes = SosTrackingRes {currentLocation :: Kernel.Prelude.Maybe SosLocationRes, sosState :: Kernel.Prelude.Maybe SosState, status :: SosStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SosType
  = Police
  | CustomerCare
  | SafetyFlow
  | CSAlertSosTicket
  | AudioRecording
  | KaptureDashboard
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("sos" :> (GetSosTracking :<|> GetSosDetails :<|> PostSosCallExternalSOS))

type GetSosTracking = (Capture "sosId" (Kernel.Types.Id.Id Dashboard.Common.Sos) :> "tracking" :> Get '[JSON] SosTrackingRes)

type GetSosDetails = (Capture "sosId" (Kernel.Types.Id.Id Dashboard.Common.Sos) :> "details" :> Get '[JSON] SosDetailsMaybeRes)

type PostSosCallExternalSOS = (Capture "sosId" (Kernel.Types.Id.Id Dashboard.Common.Sos) :> "callExternalSOS" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data SosAPIs = SosAPIs
  { getSosTracking :: Kernel.Types.Id.Id Dashboard.Common.Sos -> EulerHS.Types.EulerClient SosTrackingRes,
    getSosDetails :: Kernel.Types.Id.Id Dashboard.Common.Sos -> EulerHS.Types.EulerClient SosDetailsMaybeRes,
    postSosCallExternalSOS :: Kernel.Types.Id.Id Dashboard.Common.Sos -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkSosAPIs :: (Client EulerHS.Types.EulerClient API -> SosAPIs)
mkSosAPIs sosClient = (SosAPIs {..})
  where
    getSosTracking :<|> getSosDetails :<|> postSosCallExternalSOS = sosClient

data SosUserActionType
  = GET_SOS_TRACKING
  | GET_SOS_DETAILS
  | POST_SOS_CALL_EXTERNAL_SOS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''SosUserActionType])
