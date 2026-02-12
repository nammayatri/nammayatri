{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.ProviderPlatform.Management.Payout
  ( getPayoutPayout,
    postPayoutPayoutRetry,
    postPayoutPayoutCancel,
    postPayoutPayoutCash,
    postPayoutPayoutVpaDelete,
    postPayoutPayoutVpaUpdate,
    postPayoutPayoutVpaRefundRegistration,
  )
where

import qualified API.Client.ProviderPlatform.Management as ManagementClient
import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types as PayoutTypes
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

buildPayoutManagementServerTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildPayoutManagementServerTransaction apiTokenInfo =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

getPayoutPayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  Environment.Flow PayoutTypes.PayoutRequestResp
getPayoutPayout merchantShortId opCity apiTokenInfo payoutRequestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayout) payoutRequestId

postPayoutPayoutRetry ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutRetry merchantShortId opCity apiTokenInfo payoutRequestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetry) payoutRequestId

postPayoutPayoutCancel ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  PayoutTypes.PayoutCancelReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCancel merchantShortId opCity apiTokenInfo payoutRequestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutCancel) payoutRequestId req

postPayoutPayoutCash ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id PayoutRequest.PayoutRequest ->
  PayoutTypes.PayoutCashUpdateReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutCash merchantShortId opCity apiTokenInfo payoutRequestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutCash) payoutRequestId req

postPayoutPayoutVpaDelete ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  PayoutTypes.DeleteVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaDelete merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVpaDelete) req

postPayoutPayoutVpaUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  PayoutTypes.UpdateVpaReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVpaUpdate) req

postPayoutPayoutVpaRefundRegistration ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  PayoutTypes.RefundRegAmountReq ->
  Environment.Flow PayoutTypes.PayoutSuccess
postPayoutPayoutVpaRefundRegistration merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVpaRefundRegistration) req
