{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.SpecialZoneQueue
  ( API.Types.ProviderPlatform.Management.SpecialZoneQueue.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.SpecialZoneQueue.API)
handler merchantId city = postSpecialZoneQueueTriggerNotify merchantId city :<|> getSpecialZoneQueueQueueStats merchantId city :<|> postSpecialZoneQueueManualQueueAdd merchantId city :<|> postSpecialZoneQueueManualQueueRemove merchantId city :<|> getSpecialZoneQueueDriverQueuePosition merchantId city

postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.TriggerSpecialZoneQueueNotifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueTriggerNotify a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.postSpecialZoneQueueTriggerNotify a3 a2 a1

getSpecialZoneQueueQueueStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.SpecialZoneQueueStatsRes)
getSpecialZoneQueueQueueStats a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.getSpecialZoneQueueQueueStats a3 a2 a1

postSpecialZoneQueueManualQueueAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.ManualQueueAddReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueAdd a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.postSpecialZoneQueueManualQueueAdd a3 a2 a1

postSpecialZoneQueueManualQueueRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.ManualQueueRemoveReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueManualQueueRemove a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.postSpecialZoneQueueManualQueueRemove a3 a2 a1

getSpecialZoneQueueDriverQueuePosition :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.SpecialZoneQueue.DriverQueuePositionRes)
getSpecialZoneQueueDriverQueuePosition a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.SpecialZoneQueue.getSpecialZoneQueueDriverQueuePosition a5 a4 a3 a2 a1
