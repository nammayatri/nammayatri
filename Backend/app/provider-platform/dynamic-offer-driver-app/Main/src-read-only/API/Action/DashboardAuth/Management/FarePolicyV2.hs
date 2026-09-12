{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.FarePolicyV2
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("farePolicyV2" :> (GetFarePolicyV2List :<|> GetFarePolicyV2Policy :<|> PostFarePolicyV2PolicyReplace :<|> PostFarePolicyV2BulkReplace :<|> PostFarePolicyV2Preview :<|> PostFarePolicyV2ProductCreate :<|> PostFarePolicyV2ProductUpdate :<|> PostFarePolicyV2ProductRemove :<|> GetFarePolicyV2ChangeRequestList :<|> PostFarePolicyV2ChangeRequestDecide :<|> GetFarePolicyV2AlertsSubscriptions :<|> PostFarePolicyV2AlertsSubscribe :<|> PostFarePolicyV2AlertsUnsubscribe))

type GetFarePolicyV2List =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_LIST"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2List
  )

type GetFarePolicyV2Policy =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_POLICY"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2Policy
  )

type PostFarePolicyV2PolicyReplace =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_POLICY_REPLACE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2PolicyReplace
  )

type PostFarePolicyV2BulkReplace =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_BULK_REPLACE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2BulkReplace
  )

type PostFarePolicyV2Preview =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PREVIEW"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2Preview
  )

type PostFarePolicyV2ProductCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PRODUCT_CREATE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ProductCreate
  )

type PostFarePolicyV2ProductUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PRODUCT_UPDATE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ProductUpdate
  )

type PostFarePolicyV2ProductRemove =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_PRODUCT_REMOVE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ProductRemove
  )

type GetFarePolicyV2ChangeRequestList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_CHANGE_REQUEST_LIST"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2ChangeRequestList
  )

type PostFarePolicyV2ChangeRequestDecide =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_CHANGE_REQUEST_DECIDE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ChangeRequestDecide
  )

type GetFarePolicyV2AlertsSubscriptions =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/GET_FARE_POLICY_V2_ALERTS_SUBSCRIPTIONS"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2AlertsSubscriptions
  )

type PostFarePolicyV2AlertsSubscribe =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_ALERTS_SUBSCRIBE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2AlertsSubscribe
  )

type PostFarePolicyV2AlertsUnsubscribe =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FARE_POLICY_V2/POST_FARE_POLICY_V2_ALERTS_UNSUBSCRIBE"
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2AlertsUnsubscribe
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFarePolicyV2List merchantId city :<|> getFarePolicyV2Policy merchantId city :<|> postFarePolicyV2PolicyReplace merchantId city :<|> postFarePolicyV2BulkReplace merchantId city :<|> postFarePolicyV2Preview merchantId city :<|> postFarePolicyV2ProductCreate merchantId city :<|> postFarePolicyV2ProductUpdate merchantId city :<|> postFarePolicyV2ProductRemove merchantId city :<|> getFarePolicyV2ChangeRequestList merchantId city :<|> postFarePolicyV2ChangeRequestDecide merchantId city :<|> getFarePolicyV2AlertsSubscriptions merchantId city :<|> postFarePolicyV2AlertsSubscribe merchantId city :<|> postFarePolicyV2AlertsUnsubscribe merchantId city

getFarePolicyV2List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Dashboard.Common.TripCategory) -> Kernel.Prelude.Maybe (Lib.Types.SpecialLocation.Area) -> Kernel.Prelude.Maybe (Dashboard.Common.ServiceTierType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ProductListRes)
getFarePolicyV2List a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2List a7 a6 a4 a3 a2 a1

getFarePolicyV2Policy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PolicyRes)
getFarePolicyV2Policy a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2Policy a4 a3 a1

postFarePolicyV2PolicyReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceRes)
postFarePolicyV2PolicyReplace a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2PolicyReplace a6 a5 a3 a2 a1

postFarePolicyV2BulkReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceRes)
postFarePolicyV2BulkReplace a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2BulkReplace a5 a4 a2 a1

postFarePolicyV2Preview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewRes)
postFarePolicyV2Preview a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2Preview a4 a3 a1

postFarePolicyV2ProductCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductRes)
postFarePolicyV2ProductCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductCreate a4 a3 a1

postFarePolicyV2ProductUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2UpdateProductReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ProductUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductUpdate a5 a4 a2 a1

postFarePolicyV2ProductRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2RemoveProductReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestRes)
postFarePolicyV2ProductRemove a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ProductRemove a5 a4 a2 a1

getFarePolicyV2ChangeRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestStatus) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestListRes)
getFarePolicyV2ChangeRequestList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2ChangeRequestList a4 a3 a1

postFarePolicyV2ChangeRequestDecide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2DecideChangeRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ChangeRequestDecide a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2ChangeRequestDecide a5 a4 a2 a1

getFarePolicyV2AlertsSubscriptions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionListRes)
getFarePolicyV2AlertsSubscriptions a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.getFarePolicyV2AlertsSubscriptions a3 a2

postFarePolicyV2AlertsSubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsSubscribe a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2AlertsSubscribe a4 a3 a1

postFarePolicyV2AlertsUnsubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsUnsubscribe a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FarePolicyV2.postFarePolicyV2AlertsUnsubscribe a4 a3 a1
