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

data ManualQueueAddReq = ManualQueueAddReq {specialLocationId :: Kernel.Prelude.Text, vehicleType :: Kernel.Prelude.Text, driverId :: Kernel.Prelude.Text, queuePosition :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ManualQueueAddReq where
  hideSecrets = Kernel.Prelude.identity

data ManualQueueRemoveReq = ManualQueueRemoveReq {specialLocationId :: Kernel.Prelude.Text, vehicleType :: Kernel.Prelude.Text, driverId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ManualQueueRemoveReq where
  hideSecrets = Kernel.Prelude.identity

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

data TriggerSpecialZoneQueueNotifyReq = TriggerSpecialZoneQueueNotifyReq
  { gateId :: Kernel.Prelude.Text,
    vehicleType :: Kernel.Prelude.Text,
    driversToNotify :: Kernel.Prelude.Int,
    forceNotifyDriverIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
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

type API = ("specialZoneQueue" :> (PostSpecialZoneQueueTriggerNotify :<|> GetSpecialZoneQueueQueueStats :<|> PostSpecialZoneQueueManualQueueAdd :<|> PostSpecialZoneQueueManualQueueRemove))

type PostSpecialZoneQueueTriggerNotify = ("triggerNotify" :> ReqBody ('[JSON]) TriggerSpecialZoneQueueNotifyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type GetSpecialZoneQueueQueueStats = ("queueStats" :> Capture "gateId" Kernel.Prelude.Text :> Get ('[JSON]) SpecialZoneQueueStatsRes)

type PostSpecialZoneQueueManualQueueAdd = ("manualQueueAdd" :> ReqBody ('[JSON]) ManualQueueAddReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostSpecialZoneQueueManualQueueRemove = ("manualQueueRemove" :> ReqBody ('[JSON]) ManualQueueRemoveReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data SpecialZoneQueueAPIs = SpecialZoneQueueAPIs
  { postSpecialZoneQueueTriggerNotify :: (TriggerSpecialZoneQueueNotifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getSpecialZoneQueueQueueStats :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient SpecialZoneQueueStatsRes),
    postSpecialZoneQueueManualQueueAdd :: (ManualQueueAddReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postSpecialZoneQueueManualQueueRemove :: (ManualQueueRemoveReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkSpecialZoneQueueAPIs :: (Client EulerHS.Types.EulerClient API -> SpecialZoneQueueAPIs)
mkSpecialZoneQueueAPIs specialZoneQueueClient = (SpecialZoneQueueAPIs {..})
  where
    postSpecialZoneQueueTriggerNotify :<|> getSpecialZoneQueueQueueStats :<|> postSpecialZoneQueueManualQueueAdd :<|> postSpecialZoneQueueManualQueueRemove = specialZoneQueueClient

data SpecialZoneQueueUserActionType
  = POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY
  | GET_SPECIAL_ZONE_QUEUE_QUEUE_STATS
  | POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_ADD
  | POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_REMOVE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''SpecialZoneQueueUserActionType)])
