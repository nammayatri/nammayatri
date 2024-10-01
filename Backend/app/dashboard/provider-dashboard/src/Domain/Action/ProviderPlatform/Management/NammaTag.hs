{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.ProviderPlatform.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagQueryCreate,
    postNammaTagAppDynamicLogicVerify,
    getNammaTagAppDynamicLogic,
    postNammaTagRunJob,
    postNammaTagTagUpdate,
    deleteNammaTagTagDelete,
    postNammaTagTimeBoundsCreate,
    deleteNammaTagTimeBoundsDelete,
    getNammaTagAppDynamicLogicGetLogicRollout,
    postNammaTagAppDynamicLogicUpsertLogicRollout,
    getNammaTagTimeBounds,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.NammaTag
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI $ API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagCreateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagTagCreate) req)

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI $ API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagQueryCreateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagQueryCreate) req)

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI $ API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicVerifyEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagAppDynamicLogicVerify) req)

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.GetLogicsResp])
getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo version domain = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.getNammaTagAppDynamicLogic) version domain

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.Flow Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagRunJobEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagRunJob) req)

--- << AUTOGENERATED Check this code, update export list and remove comment >> ---

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagUpdateEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagTagUpdate) req)

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete merchantShortId opCity apiTokenInfo tagName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagTagDeleteEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.deleteNammaTagTagDelete) tagName)

--- << AUTOGENERATED Check this code, update export list and remove comment >> ---

postNammaTagTimeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTimeBoundsCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTimeBoundsCreateEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagTimeBoundsCreate) req)

deleteNammaTagTimeBoundsDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTimeBoundsDelete merchantShortId opCity apiTokenInfo domain name = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagTimeBoundsDeleteEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.deleteNammaTagTimeBoundsDelete) domain name)

getNammaTagAppDynamicLogicGetLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow [Lib.Yudhishthira.Types.LogicRolloutObject])
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity apiTokenInfo timeBound domain = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.getNammaTagAppDynamicLogicGetLogicRollout) timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ProviderManagementAPI (API.Types.ProviderPlatform.Management.NammaTagAPI API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpsertLogicRolloutEndpoint)) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.postNammaTagAppDynamicLogicUpsertLogicRollout) req)

--- << AUTOGENERATED Check this code, update export list and remove comment >> ---

getNammaTagTimeBounds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Environment.Flow Lib.Yudhishthira.Types.TimeBoundResp)
getNammaTagTimeBounds merchantShortId opCity apiTokenInfo domain = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.nammaTagDSL.getNammaTagTimeBounds) domain
