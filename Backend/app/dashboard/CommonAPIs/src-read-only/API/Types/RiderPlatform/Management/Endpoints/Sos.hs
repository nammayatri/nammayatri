{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Sos where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
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

type API = ("sos" :> GetSosTracking)

type GetSosTracking = (Capture "sosId" (Kernel.Types.Id.Id Dashboard.Common.Sos) :> "tracking" :> Get '[JSON] SosTrackingRes)

newtype SosAPIs = SosAPIs {getSosTracking :: Kernel.Types.Id.Id Dashboard.Common.Sos -> EulerHS.Types.EulerClient SosTrackingRes}

mkSosAPIs :: (Client EulerHS.Types.EulerClient API -> SosAPIs)
mkSosAPIs sosClient = (SosAPIs {..})
  where
    getSosTracking = sosClient

data SosUserActionType
  = GET_SOS_TRACKING
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SosUserActionType where
  toJSON GET_SOS_TRACKING = Data.Aeson.String "GET_SOS_TRACKING"

instance FromJSON SosUserActionType where
  parseJSON (Data.Aeson.String "GET_SOS_TRACKING") = pure GET_SOS_TRACKING
  parseJSON _ = fail "GET_SOS_TRACKING expected"

$(Data.Singletons.TH.genSingletons [''SosUserActionType])
