{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.SpecialZoneQueue where

import qualified Data.Aeson
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

data TriggerSpecialZoneQueueNotifyReq = TriggerSpecialZoneQueueNotifyReq {gateId :: Kernel.Prelude.Text, vehicleType :: Kernel.Prelude.Text, driversToNotify :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TriggerSpecialZoneQueueNotifyReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("specialZoneQueue" :> PostSpecialZoneQueueTriggerNotify)

type PostSpecialZoneQueueTriggerNotify = ("triggerNotify" :> ReqBody ('[JSON]) TriggerSpecialZoneQueueNotifyReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

newtype SpecialZoneQueueAPIs = SpecialZoneQueueAPIs {postSpecialZoneQueueTriggerNotify :: (TriggerSpecialZoneQueueNotifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkSpecialZoneQueueAPIs :: (Client EulerHS.Types.EulerClient API -> SpecialZoneQueueAPIs)
mkSpecialZoneQueueAPIs specialZoneQueueClient = (SpecialZoneQueueAPIs {..})
  where
    postSpecialZoneQueueTriggerNotify = specialZoneQueueClient

data SpecialZoneQueueUserActionType
  = POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SpecialZoneQueueUserActionType where
  toJSON (POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY) = Data.Aeson.String "POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY"

instance FromJSON SpecialZoneQueueUserActionType where
  parseJSON (Data.Aeson.String "POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY") = pure POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY
  parseJSON _ = fail "POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY expected"

$(Data.Singletons.TH.genSingletons [(''SpecialZoneQueueUserActionType)])
