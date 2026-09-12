module Domain.Action.ProviderPlatform.Management.MediaFileDocument
  ( postMediaFileDocumentUploadLink,
    postMediaFileDocumentConfirm,
    postMediaFileDocumentDelete,
    getMediaFileDocumentDownloadLink,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dynamic-offer-driver-app" API.Types.ProviderPlatform.Management.MediaFileDocument as Common
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import qualified "lib-dashboard" SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

postMediaFileDocumentUploadLink ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo UserActionType ->
  Common.UploadMediaFileDocumentReq ->
  Flow Common.MediaFileDocumentResp
postMediaFileDocumentUploadLink merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.ActionAPI apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  let requestorId = apiTokenInfo.personId.getId
  T.withResponseTransactionStoring transaction $ do
    Client.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.postMediaFileDocumentUploadLink) requestorId req

postMediaFileDocumentConfirm ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo UserActionType ->
  Common.MediaFileDocumentReq ->
  Flow APISuccess
postMediaFileDocumentConfirm merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.ActionAPI apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  let requestorId = apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ do
    Client.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.postMediaFileDocumentConfirm) requestorId req

postMediaFileDocumentDelete ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo UserActionType ->
  Common.MediaFileDocumentReq ->
  Flow APISuccess
postMediaFileDocumentDelete merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- T.buildTransaction (DT.ActionAPI apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  let requestorId = apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ do
    Client.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.postMediaFileDocumentDelete) requestorId req

getMediaFileDocumentDownloadLink ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo UserActionType ->
  Text ->
  Flow Common.MediaFileDocumentResp
getMediaFileDocumentDownloadLink merchantShortId opCity apiTokenInfo fileId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callManagementAPI checkedMerchantId opCity (.mediaFileDocumentDSL.getMediaFileDocumentDownloadLink) fileId requestorId
