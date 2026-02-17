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

type API = ("sos" :> (GetSosTracking :<|> PostSosCallExternalSOS))

type GetSosTracking = (Capture "sosId" (Kernel.Types.Id.Id Dashboard.Common.Sos) :> "tracking" :> Get '[JSON] SosTrackingRes)

type PostSosCallExternalSOS = (Capture "sosId" (Kernel.Types.Id.Id Dashboard.Common.Sos) :> "callExternalSOS" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data SosAPIs = SosAPIs
  { getSosTracking :: Kernel.Types.Id.Id Dashboard.Common.Sos -> EulerHS.Types.EulerClient SosTrackingRes,
    postSosCallExternalSOS :: Kernel.Types.Id.Id Dashboard.Common.Sos -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkSosAPIs :: (Client EulerHS.Types.EulerClient API -> SosAPIs)
mkSosAPIs sosClient = (SosAPIs {..})
  where
    getSosTracking :<|> postSosCallExternalSOS = sosClient

data SosUserActionType
  = GET_SOS_TRACKING
  | POST_SOS_CALL_EXTERNAL_SOS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''SosUserActionType])
