{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.ProviderPlatform.Management.Payout
  ( getPayoutPayout,
    getPayoutPayoutHistory,
    getPayoutPayoutReferralHistory,
    postPayoutPayoutRetry,
    postPayoutPayoutCancel,
    postPayoutPayoutCash,
    postPayoutPayoutVpaDelete,
    postPayoutPayoutVpaUpdate,
    postPayoutPayoutVpaRefundRegistration,
    postPayoutPayoutScheduledPayoutConfigUpsert,
  )
where

import qualified API.Client.ProviderPlatform.Management as ManagementClient
import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.API.Payout.Types as PayoutTypes
import qualified "payment" Lib.Payment.Domain.Types.PayoutRequest as PayoutRequest
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

instance Common.HideSecrets ApiPayout.UpdateScheduledPayoutConfigReq where
  hideSecrets = identity

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

getPayoutPayoutHistory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Environment.Flow PayoutTypes.PayoutHistoryRes
getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutHistory) driverId driverPhoneNo from isFailedOnly limit offset to

getPayoutPayoutReferralHistory ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Maybe Bool ->
  Maybe Text ->
  Maybe (Kernel.Types.Id.Id Common.Driver) ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Environment.Flow ApiPayout.PayoutReferralHistoryRes
getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneNo from limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutReferralHistory) areActivatedRidesOnly customerPhoneNo driverId driverPhoneNo from limit offset to

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

postPayoutPayoutScheduledPayoutConfigUpsert ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  ApiPayout.UpdateScheduledPayoutConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutPayoutScheduledPayoutConfigUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $ do
    ManagementClient.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutScheduledPayoutConfigUpsert) req
