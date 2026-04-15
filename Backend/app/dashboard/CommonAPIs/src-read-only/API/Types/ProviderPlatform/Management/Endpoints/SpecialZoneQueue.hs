{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.SpecialZoneQueue where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data SpecialZoneQueueStatsRes = SpecialZoneQueueStatsRes
  { gateId :: Kernel.Prelude.Text,
    gateName :: Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Text,
    canQueueUpOnGate :: Kernel.Prelude.Bool,
    vehicleStats :: [VehicleQueueStats],
    totalDriversInQueue :: Kernel.Prelude.Int,
    totalInPickupZone :: Kernel.Prelude.Int,
    totalOutsidePickupZone :: Kernel.Prelude.Int,
    totalPendingDemand :: Kernel.Prelude.Int,
    totalDriversCommittedToPickup :: Kernel.Prelude.Int,
    totalAcceptedQueueRequests :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TriggerSpecialZoneQueueNotifyReq = TriggerSpecialZoneQueueNotifyReq {gateId :: Kernel.Prelude.Text, vehicleType :: Kernel.Prelude.Text, driversToNotify :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TriggerSpecialZoneQueueNotifyReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleQueueStats = VehicleQueueStats
  { vehicleType :: Kernel.Prelude.Text,
    totalInQueue :: Kernel.Prelude.Int,
    inPickupZone :: Kernel.Prelude.Int,
    outsidePickupZone :: Kernel.Prelude.Int,
    pendingDemand :: Kernel.Prelude.Int,
    driversCommittedToPickup :: Kernel.Prelude.Int,
    acceptedQueueRequests :: Kernel.Prelude.Int,
    demandThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minDriverThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxDriverThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("specialZoneQueue" :> (PostSpecialZoneQueueTriggerNotify :<|> GetSpecialZoneQueueQueueStats))

type PostSpecialZoneQueueTriggerNotify = ("triggerNotify" :> ReqBody ('[JSON]) TriggerSpecialZoneQueueNotifyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type GetSpecialZoneQueueQueueStats = ("queueStats" :> Capture "gateId" Kernel.Prelude.Text :> Get ('[JSON]) SpecialZoneQueueStatsRes)

data SpecialZoneQueueAPIs = SpecialZoneQueueAPIs
  { postSpecialZoneQueueTriggerNotify :: (TriggerSpecialZoneQueueNotifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getSpecialZoneQueueQueueStats :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient SpecialZoneQueueStatsRes)
  }

mkSpecialZoneQueueAPIs :: (Client EulerHS.Types.EulerClient API -> SpecialZoneQueueAPIs)
mkSpecialZoneQueueAPIs specialZoneQueueClient = (SpecialZoneQueueAPIs {..})
  where
    postSpecialZoneQueueTriggerNotify :<|> getSpecialZoneQueueQueueStats = specialZoneQueueClient

data SpecialZoneQueueUserActionType
  = POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY
  | GET_SPECIAL_ZONE_QUEUE_QUEUE_STATS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''SpecialZoneQueueUserActionType)])
