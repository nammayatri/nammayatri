module Domain.Action.ProviderPlatform.AppManagement.Overlay
  ( postOverlayCreate,
    postOverlayDelete,
    getOverlayList,
    getOverlayInfo,
    postOverlaySchedule,
  )
where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.Overlay
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

postOverlayCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  API.Types.Dashboard.AppManagement.Overlay.CreateOverlayReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postOverlayCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.overlayDSL.postOverlayCreate)
      req

postOverlayDelete ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  API.Types.Dashboard.AppManagement.Overlay.DeleteOverlayReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postOverlayDelete merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.overlayDSL.postOverlayDelete)
      req

getOverlayList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Environment.Flow API.Types.Dashboard.AppManagement.Overlay.ListOverlayResp
getOverlayList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.overlayDSL.getOverlayList)

getOverlayInfo ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow API.Types.Dashboard.AppManagement.Overlay.OverlayInfoResp
getOverlayInfo merchantShortId opCity apiTokenInfo udf1 overlayKey = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
    checkedMerchantId
    opCity
    (.overlayDSL.getOverlayInfo)
    udf1
    overlayKey

postOverlaySchedule ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  API.Types.Dashboard.AppManagement.Overlay.ScheduleOverlay ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postOverlaySchedule merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <-
    SharedLogic.Transaction.buildTransaction
      (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType)
      (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT)
      (Kernel.Prelude.Just apiTokenInfo)
      Kernel.Prelude.Nothing
      Kernel.Prelude.Nothing
      (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.ProviderPlatform.AppManagement.callAppManagementAPI
      checkedMerchantId
      opCity
      (.overlayDSL.postOverlaySchedule)
      req
