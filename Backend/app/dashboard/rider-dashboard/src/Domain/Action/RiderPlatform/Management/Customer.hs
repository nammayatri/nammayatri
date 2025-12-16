{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.RiderPlatform.Management.Customer
  ( getCustomerList,
    deleteCustomerDelete,
    postCustomerBlock,
    postCustomerUnblock,
    getCustomerInfo,
    postCustomerCancellationDuesSync,
    getCustomerCancellationDuesDetails,
    postCustomerUpdateSafetyCenterBlocking,
    postCustomerPersonNumbers,
    postCustomerPersonId,
    postCustomerUpdatePaymentMode,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Customer
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getCustomerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Environment.Flow API.Types.RiderPlatform.Management.Customer.CustomerListRes)
getCustomerList merchantShortId opCity apiTokenInfo limit offset enabled blocked phone personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.getCustomerList) limit offset enabled blocked phone personId

deleteCustomerDelete ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Customer ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteCustomerDelete merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.deleteCustomerDelete) customerId

postCustomerBlock ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Customer ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCustomerBlock merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.postCustomerBlock) customerId

postCustomerUnblock :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postCustomerUnblock merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.postCustomerUnblock) customerId

getCustomerInfo :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.Flow API.Types.RiderPlatform.Management.Customer.CustomerInfoRes
getCustomerInfo merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.getCustomerInfo) customerId

postCustomerCancellationDuesSync ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Customer ->
  API.Types.RiderPlatform.Management.Customer.CustomerCancellationDuesSyncReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCustomerCancellationDuesSync merchantShortId opCity apiTokenInfo customerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.postCustomerCancellationDuesSync) customerId req

getCustomerCancellationDuesDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.Flow API.Types.RiderPlatform.Management.Customer.CancellationDuesDetailsRes
getCustomerCancellationDuesDetails merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.getCustomerCancellationDuesDetails) customerId

postCustomerUpdateSafetyCenterBlocking :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdateSafetyCenterBlockingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postCustomerUpdateSafetyCenterBlocking merchantShortId opCity apiTokenInfo customerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.postCustomerUpdateSafetyCenterBlocking) customerId req

postCustomerPersonId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.PersonMobileNoReq -> Environment.Flow [Dashboard.Common.PersonRes]
postCustomerPersonId merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.customerDSL.postCustomerPersonId)) req

postCustomerPersonNumbers :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.PersonIdsReq -> Environment.Flow [Dashboard.Common.PersonRes]
postCustomerPersonNumbers merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.customerDSL.postCustomerPersonNumbers)) req

postCustomerUpdatePaymentMode :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdatePaymentModeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postCustomerUpdatePaymentMode merchantShortId opCity apiTokenInfo customerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction do
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.customerDSL.postCustomerUpdatePaymentMode) customerId req
