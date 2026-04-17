{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.SpecialZoneQueue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.SpecialZoneQueue
import qualified Domain.Action.ProviderPlatform.Management.SpecialZoneQueue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("specialZoneQueue" :> (PostSpecialZoneQueueTriggerNotify :<|> GetSpecialZoneQueueQueueStats :<|> PostSpecialZoneQueueManualQueueAdd :<|> PostSpecialZoneQueueManualQueueRemove))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSpecialZoneQueueTriggerNotify merchantId city :<|> getSpecialZoneQueueQueueStats merchantId city :<|> postSpecialZoneQueueManualQueueAdd merchantId city :<|> postSpecialZoneQueueManualQueueRemove merchantId city

type PostSpecialZoneQueueTriggerNotify =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SPECIAL_ZONE_QUEUE) / ('API.Types.ProviderPlatform.Management.SpecialZoneQueue.POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY))
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.PostSpecialZoneQueueTriggerNotify
  )

type GetSpecialZoneQueueQueueStats =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SPECIAL_ZONE_QUEUE) / ('API.Types.ProviderPlatform.Management.SpecialZoneQueue.GET_SPECIAL_ZONE_QUEUE_QUEUE_STATS))
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.GetSpecialZoneQueueQueueStats
  )

type PostSpecialZoneQueueManualQueueAdd =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SPECIAL_ZONE_QUEUE) / ('API.Types.ProviderPlatform.Management.SpecialZoneQueue.POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_ADD))
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.PostSpecialZoneQueueManualQueueAdd
  )

type PostSpecialZoneQueueManualQueueRemove =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SPECIAL_ZONE_QUEUE) / ('API.Types.ProviderPlatform.Management.SpecialZoneQueue.POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_REMOVE))
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.PostSpecialZoneQueueManualQueueRemove
  )

postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.TriggerSpecialZoneQueueNotifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueTriggerNotify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SpecialZoneQueue.postSpecialZoneQueueTriggerNotify merchantShortId opCity apiTokenInfo req

getSpecialZoneQueueQueueStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.SpecialZoneQueueStatsRes)
getSpecialZoneQueueQueueStats merchantShortId opCity apiTokenInfo gateId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SpecialZoneQueue.getSpecialZoneQueueQueueStats merchantShortId opCity apiTokenInfo gateId

postSpecialZoneQueueManualQueueAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.ManualQueueAddReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueAdd merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SpecialZoneQueue.postSpecialZoneQueueManualQueueAdd merchantShortId opCity apiTokenInfo req

postSpecialZoneQueueManualQueueRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.ManualQueueRemoveReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueRemove merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SpecialZoneQueue.postSpecialZoneQueueManualQueueRemove merchantShortId opCity apiTokenInfo req
