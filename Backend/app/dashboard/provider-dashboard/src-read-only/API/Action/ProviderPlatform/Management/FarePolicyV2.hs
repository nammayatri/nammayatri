{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.FarePolicyV2
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FarePolicyV2
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.FarePolicyV2
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Types.SpecialLocation
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("farePolicyV2" :> (GetFarePolicyV2List :<|> GetFarePolicyV2Policy :<|> PostFarePolicyV2PolicyReplace :<|> PostFarePolicyV2BulkReplace :<|> PostFarePolicyV2Preview :<|> PostFarePolicyV2ProductCreate :<|> PostFarePolicyV2ProductUpdate :<|> PostFarePolicyV2ProductRemove :<|> GetFarePolicyV2ChangeRequestList :<|> PostFarePolicyV2ChangeRequestDecide :<|> GetFarePolicyV2AlertsSubscriptions :<|> PostFarePolicyV2AlertsSubscribe :<|> PostFarePolicyV2AlertsUnsubscribe))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFarePolicyV2List merchantId city :<|> getFarePolicyV2Policy merchantId city :<|> postFarePolicyV2PolicyReplace merchantId city :<|> postFarePolicyV2BulkReplace merchantId city :<|> postFarePolicyV2Preview merchantId city :<|> postFarePolicyV2ProductCreate merchantId city :<|> postFarePolicyV2ProductUpdate merchantId city :<|> postFarePolicyV2ProductRemove merchantId city :<|> getFarePolicyV2ChangeRequestList merchantId city :<|> postFarePolicyV2ChangeRequestDecide merchantId city :<|> getFarePolicyV2AlertsSubscriptions merchantId city :<|> postFarePolicyV2AlertsSubscribe merchantId city :<|> postFarePolicyV2AlertsUnsubscribe merchantId city

type GetFarePolicyV2List =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.GET_FARE_POLICY_V2_LIST))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2List
  )

type GetFarePolicyV2Policy =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.GET_FARE_POLICY_V2_POLICY))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2Policy
  )

type PostFarePolicyV2PolicyReplace =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_POLICY_REPLACE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2PolicyReplace
  )

type PostFarePolicyV2BulkReplace =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_BULK_REPLACE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2BulkReplace
  )

type PostFarePolicyV2Preview =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_PREVIEW))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2Preview
  )

type PostFarePolicyV2ProductCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_PRODUCT_CREATE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ProductCreate
  )

type PostFarePolicyV2ProductUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_PRODUCT_UPDATE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ProductUpdate
  )

type PostFarePolicyV2ProductRemove =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_PRODUCT_REMOVE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ProductRemove
  )

type GetFarePolicyV2ChangeRequestList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.GET_FARE_POLICY_V2_CHANGE_REQUEST_LIST))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2ChangeRequestList
  )

type PostFarePolicyV2ChangeRequestDecide =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_CHANGE_REQUEST_DECIDE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2ChangeRequestDecide
  )

type GetFarePolicyV2AlertsSubscriptions =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.GET_FARE_POLICY_V2_ALERTS_SUBSCRIPTIONS))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.GetFarePolicyV2AlertsSubscriptions
  )

type PostFarePolicyV2AlertsSubscribe =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_ALERTS_SUBSCRIBE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2AlertsSubscribe
  )

type PostFarePolicyV2AlertsUnsubscribe =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FARE_POLICY_V2) / ('API.Types.ProviderPlatform.Management.FarePolicyV2.POST_FARE_POLICY_V2_ALERTS_UNSUBSCRIBE))
      :> API.Types.ProviderPlatform.Management.FarePolicyV2.PostFarePolicyV2AlertsUnsubscribe
  )

getFarePolicyV2List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Dashboard.Common.TripCategory) -> Kernel.Prelude.Maybe (Lib.Types.SpecialLocation.Area) -> Kernel.Prelude.Maybe (Dashboard.Common.ServiceTierType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ProductListRes)
getFarePolicyV2List merchantShortId opCity apiTokenInfo tripCategory area serviceTier enabled = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.getFarePolicyV2List merchantShortId opCity apiTokenInfo tripCategory area serviceTier enabled

getFarePolicyV2Policy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PolicyRes)
getFarePolicyV2Policy merchantShortId opCity apiTokenInfo farePolicyId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.getFarePolicyV2Policy merchantShortId opCity apiTokenInfo farePolicyId

postFarePolicyV2PolicyReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceRes)
postFarePolicyV2PolicyReplace merchantShortId opCity apiTokenInfo farePolicyId dryRun req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2PolicyReplace merchantShortId opCity apiTokenInfo farePolicyId dryRun req

postFarePolicyV2BulkReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceRes)
postFarePolicyV2BulkReplace merchantShortId opCity apiTokenInfo dryRun req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2BulkReplace merchantShortId opCity apiTokenInfo dryRun req

postFarePolicyV2Preview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewRes)
postFarePolicyV2Preview merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2Preview merchantShortId opCity apiTokenInfo req

postFarePolicyV2ProductCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductRes)
postFarePolicyV2ProductCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2ProductCreate merchantShortId opCity apiTokenInfo req

postFarePolicyV2ProductUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2UpdateProductReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ProductUpdate merchantShortId opCity apiTokenInfo fareProductId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2ProductUpdate merchantShortId opCity apiTokenInfo fareProductId req

postFarePolicyV2ProductRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2RemoveProductReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestRes)
postFarePolicyV2ProductRemove merchantShortId opCity apiTokenInfo fareProductId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2ProductRemove merchantShortId opCity apiTokenInfo fareProductId req

getFarePolicyV2ChangeRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestStatus) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestListRes)
getFarePolicyV2ChangeRequestList merchantShortId opCity apiTokenInfo status = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.getFarePolicyV2ChangeRequestList merchantShortId opCity apiTokenInfo status

postFarePolicyV2ChangeRequestDecide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2DecideChangeRequestReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ChangeRequestDecide merchantShortId opCity apiTokenInfo requestId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2ChangeRequestDecide merchantShortId opCity apiTokenInfo requestId req

getFarePolicyV2AlertsSubscriptions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionListRes)
getFarePolicyV2AlertsSubscriptions merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.getFarePolicyV2AlertsSubscriptions merchantShortId opCity apiTokenInfo

postFarePolicyV2AlertsSubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsSubscribe merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2AlertsSubscribe merchantShortId opCity apiTokenInfo req

postFarePolicyV2AlertsUnsubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsUnsubscribe merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FarePolicyV2.postFarePolicyV2AlertsUnsubscribe merchantShortId opCity apiTokenInfo req
