{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.ProviderPlatform.Management.Payout
  ( getPayoutPayoutReferralHistory,
    getPayoutPayoutHistory,
    postPayoutPayoutVerifyFraudStatus,
    postPayoutPayoutRetryFailed,
    postPayoutPayoutRetryAllWithStatus,
    postPayoutPayoutPendingPayout,
    postPayoutPayoutDeleteVPA,
    postPayoutPayoutDriversSetBlockState,
    postPayoutPayoutUpdateVPA,
    postPayoutPayoutRefundRegistrationAmount,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Payout
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

buildPayoutManagementServerTransaction ::
  ( MonadFlow m,
    Dashboard.Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildPayoutManagementServerTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) driverId Nothing

getPayoutPayoutReferralHistory :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes
getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo driverId driverPhoneNo from limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutReferralHistory) areActivatedRidesOnly customerPhoneNo driverId driverPhoneNo from limit offset to

getPayoutPayoutHistory :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.Payout.PayoutHistoryRes
getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.getPayoutPayoutHistory) driverId driverPhoneNo from isFailedOnly limit offset to

postPayoutPayoutVerifyFraudStatus :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.UpdateFraudStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutPayoutVerifyFraudStatus merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo (Just req.driverId) (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVerifyFraudStatus) req

postPayoutPayoutRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.FailedRetryPayoutReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryFailed merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetryFailed) req

postPayoutPayoutRetryAllWithStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.RetryPayoutsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryAllWithStatus merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetryAllWithStatus) req

postPayoutPayoutPendingPayout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.PendingPayoutReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutPendingPayout merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutPendingPayout) req

postPayoutPayoutDeleteVPA :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.DeleteVpaReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutDeleteVPA merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutDeleteVPA) req

postPayoutPayoutDriversSetBlockState :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.SetDriversBlockStateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutDriversSetBlockState merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutDriversSetBlockState) req

postPayoutPayoutUpdateVPA :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.UpdateVpaReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutUpdateVPA merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutUpdateVPA) req

postPayoutPayoutRefundRegistrationAmount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.RefundRegAmountReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRefundRegistrationAmount merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRefundRegistrationAmount) req
