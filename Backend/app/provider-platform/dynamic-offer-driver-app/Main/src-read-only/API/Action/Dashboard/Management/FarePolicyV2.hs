{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.FarePolicyV2
  ( API.Types.ProviderPlatform.Management.FarePolicyV2.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.FarePolicyV2
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.FarePolicyV2
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.FarePolicyV2.API)
handler merchantId city = getFarePolicyV2List merchantId city :<|> getFarePolicyV2Policy merchantId city :<|> postFarePolicyV2PolicyReplace merchantId city :<|> postFarePolicyV2BulkReplace merchantId city :<|> postFarePolicyV2Preview merchantId city :<|> postFarePolicyV2ProductCreate merchantId city :<|> postFarePolicyV2ProductUpdate merchantId city :<|> postFarePolicyV2ProductRemove merchantId city :<|> getFarePolicyV2ChangeRequestList merchantId city :<|> postFarePolicyV2ChangeRequestDecide merchantId city :<|> getFarePolicyV2AlertsSubscriptions merchantId city :<|> postFarePolicyV2AlertsSubscribe merchantId city :<|> postFarePolicyV2AlertsUnsubscribe merchantId city

getFarePolicyV2List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Dashboard.Common.TripCategory) -> Kernel.Prelude.Maybe (Lib.Types.SpecialLocation.Area) -> Kernel.Prelude.Maybe (Dashboard.Common.ServiceTierType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ProductListRes)
getFarePolicyV2List a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2List a6 a5 a4 a3 a2 a1

getFarePolicyV2Policy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PolicyRes)
getFarePolicyV2Policy a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2Policy a3 a2 a1

postFarePolicyV2PolicyReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceRes)
postFarePolicyV2PolicyReplace a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2PolicyReplace a5 a4 a3 a2 a1

postFarePolicyV2BulkReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceRes)
postFarePolicyV2BulkReplace a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2BulkReplace a4 a3 a2 a1

postFarePolicyV2Preview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewRes)
postFarePolicyV2Preview a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2Preview a3 a2 a1

postFarePolicyV2ProductCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductRes)
postFarePolicyV2ProductCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductCreate a3 a2 a1

postFarePolicyV2ProductUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2UpdateProductReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ProductUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductUpdate a4 a3 a2 a1

postFarePolicyV2ProductRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2RemoveProductReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestRes)
postFarePolicyV2ProductRemove a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductRemove a4 a3 a2 a1

getFarePolicyV2ChangeRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestStatus) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestListRes)
getFarePolicyV2ChangeRequestList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2ChangeRequestList a3 a2 a1

postFarePolicyV2ChangeRequestDecide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2DecideChangeRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ChangeRequestDecide a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ChangeRequestDecide a4 a3 a2 a1

getFarePolicyV2AlertsSubscriptions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionListRes)
getFarePolicyV2AlertsSubscriptions a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2AlertsSubscriptions a2 a1

postFarePolicyV2AlertsSubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsSubscribe a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2AlertsSubscribe a3 a2 a1

postFarePolicyV2AlertsUnsubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsUnsubscribe a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2AlertsUnsubscribe a3 a2 a1
