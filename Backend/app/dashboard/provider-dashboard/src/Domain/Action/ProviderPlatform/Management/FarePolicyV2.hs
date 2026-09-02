{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.FarePolicyV2
  ( getFarePolicyV2List,
    getFarePolicyV2Policy,
    postFarePolicyV2PolicyReplace,
    postFarePolicyV2BulkReplace,
    postFarePolicyV2Preview,
    postFarePolicyV2ProductCreate,
    postFarePolicyV2ProductUpdate,
    postFarePolicyV2ProductRemove,
    getFarePolicyV2ChangeRequestList,
    postFarePolicyV2ChangeRequestDecide,
    getFarePolicyV2AlertsSubscriptions,
    postFarePolicyV2AlertsSubscribe,
    postFarePolicyV2AlertsUnsubscribe,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FarePolicyV2
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getFarePolicyV2List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Dashboard.Common.TripCategory -> Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area -> Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ProductListRes)
getFarePolicyV2List merchantShortId opCity apiTokenInfo tripCategory area serviceTier enabled = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.getFarePolicyV2List) tripCategory area serviceTier enabled

getFarePolicyV2Policy :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PolicyRes)
getFarePolicyV2Policy merchantShortId opCity apiTokenInfo farePolicyId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.getFarePolicyV2Policy) farePolicyId

postFarePolicyV2PolicyReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceReq -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ReplaceRes)
postFarePolicyV2PolicyReplace merchantShortId opCity apiTokenInfo farePolicyId dryRun req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2PolicyReplace) farePolicyId dryRun req)

postFarePolicyV2BulkReplace :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceReq -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2BulkReplaceRes)
postFarePolicyV2BulkReplace merchantShortId opCity apiTokenInfo dryRun req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2BulkReplace) dryRun req)

postFarePolicyV2Preview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewReq -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2PreviewRes)
postFarePolicyV2Preview merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2Preview) req

postFarePolicyV2ProductCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductReq -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2CreateProductRes)
postFarePolicyV2ProductCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2ProductCreate) req)

postFarePolicyV2ProductUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2UpdateProductReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ProductUpdate merchantShortId opCity apiTokenInfo fareProductId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2ProductUpdate) fareProductId req)

-- maker identity is stamped from the authenticated token, never trusted from the client
postFarePolicyV2ProductRemove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FareProduct -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2RemoveProductReq -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestRes)
postFarePolicyV2ProductRemove merchantShortId opCity apiTokenInfo fareProductId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let req' = req {API.Types.ProviderPlatform.Management.FarePolicyV2.requestedBy = Kernel.Prelude.Just apiTokenInfo.personId.getId} :: API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2RemoveProductReq
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req')
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2ProductRemove) fareProductId req')

getFarePolicyV2ChangeRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestStatus -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2ChangeRequestListRes)
getFarePolicyV2ChangeRequestList merchantShortId opCity apiTokenInfo status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.getFarePolicyV2ChangeRequestList) status

-- checker identity is stamped from the authenticated token; the BPP rejects maker == checker
postFarePolicyV2ChangeRequestDecide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2DecideChangeRequestReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2ChangeRequestDecide merchantShortId opCity apiTokenInfo requestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let req' = req {API.Types.ProviderPlatform.Management.FarePolicyV2.checkedBy = Kernel.Prelude.Just apiTokenInfo.personId.getId} :: API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2DecideChangeRequestReq
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req')
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2ChangeRequestDecide) requestId req')

getFarePolicyV2AlertsSubscriptions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionListRes)
getFarePolicyV2AlertsSubscriptions merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.getFarePolicyV2AlertsSubscriptions)

postFarePolicyV2AlertsSubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsSubscribe merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2AlertsSubscribe) req)

postFarePolicyV2AlertsUnsubscribe :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FarePolicyV2.FPV2SubscriptionReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFarePolicyV2AlertsUnsubscribe merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.farePolicyV2DSL.postFarePolicyV2AlertsUnsubscribe) req)
