{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.SpecialZoneQueue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.SpecialZoneQueue
import qualified Domain.Action.Dashboard.Management.SpecialZoneQueue
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("specialZoneQueue" :> (PostSpecialZoneQueueTriggerNotify :<|> GetSpecialZoneQueueTriggerNotifyStatus :<|> GetSpecialZoneQueueQueueStats :<|> PostSpecialZoneQueueManualQueueAdd :<|> PostSpecialZoneQueueManualQueueRemove :<|> GetSpecialZoneQueueDriverQueuePosition :<|> GetSpecialZoneQueueDriverQueueHistory))

type PostSpecialZoneQueueTriggerNotify =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.PostSpecialZoneQueueTriggerNotify
  )

type GetSpecialZoneQueueTriggerNotifyStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY_STATUS"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.GetSpecialZoneQueueTriggerNotifyStatus
  )

type GetSpecialZoneQueueQueueStats =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_QUEUE_STATS"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.GetSpecialZoneQueueQueueStats
  )

type PostSpecialZoneQueueManualQueueAdd =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_ADD"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.PostSpecialZoneQueueManualQueueAdd
  )

type PostSpecialZoneQueueManualQueueRemove =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_REMOVE"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.PostSpecialZoneQueueManualQueueRemove
  )

type GetSpecialZoneQueueDriverQueuePosition =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_DRIVER_QUEUE_POSITION"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.GetSpecialZoneQueueDriverQueuePosition
  )

type GetSpecialZoneQueueDriverQueueHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_DRIVER_QUEUE_HISTORY"
      :> API.Types.ProviderPlatform.Management.SpecialZoneQueue.GetSpecialZoneQueueDriverQueueHistory
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSpecialZoneQueueTriggerNotify merchantId city :<|> getSpecialZoneQueueTriggerNotifyStatus merchantId city :<|> getSpecialZoneQueueQueueStats merchantId city :<|> postSpecialZoneQueueManualQueueAdd merchantId city :<|> postSpecialZoneQueueManualQueueRemove merchantId city :<|> getSpecialZoneQueueDriverQueuePosition merchantId city :<|> getSpecialZoneQueueDriverQueueHistory merchantId city

postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.TriggerSpecialZoneQueueNotifyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.TriggerSpecialZoneQueueNotifyRes)
postSpecialZoneQueueTriggerNotify a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.postSpecialZoneQueueTriggerNotify a4 a3 a1

getSpecialZoneQueueTriggerNotifyStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.TriggerSpecialZoneQueueNotifyStatusRes)
getSpecialZoneQueueTriggerNotifyStatus a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.getSpecialZoneQueueTriggerNotifyStatus a3 a2

getSpecialZoneQueueQueueStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.SpecialZoneQueueStatsRes)
getSpecialZoneQueueQueueStats a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.getSpecialZoneQueueQueueStats a4 a3 a1

postSpecialZoneQueueManualQueueAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.ManualQueueAddReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueAdd a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.postSpecialZoneQueueManualQueueAdd a4 a3 a1

postSpecialZoneQueueManualQueueRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.ManualQueueRemoveReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueRemove a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.postSpecialZoneQueueManualQueueRemove a4 a3 a1

getSpecialZoneQueueDriverQueuePosition :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.DriverQueuePositionRes)
getSpecialZoneQueueDriverQueuePosition a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.getSpecialZoneQueueDriverQueuePosition a6 a5 a3 a2 a1

getSpecialZoneQueueDriverQueueHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.DriverQueueHistoryRes)
getSpecialZoneQueueDriverQueueHistory a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.getSpecialZoneQueueDriverQueueHistory a4 a3 a1
