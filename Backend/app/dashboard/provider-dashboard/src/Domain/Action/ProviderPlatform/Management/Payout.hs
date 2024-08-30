{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.ProviderPlatform.Management.Payout
  ( getPayoutPayoutReferralHistory,
    getPayoutPayoutHistory,
    postPayoutPayoutVerifyFraudStatus,
    postPayoutPayoutRetryFailed,
    postPayoutPayoutRetryAllWithStatus,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management
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
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

buildPayoutManagementServerTransaction ::
  ( MonadFlow m,
    Dashboard.Common.HideSecrets request
  ) =>
  API.Types.ProviderPlatform.Management.Payout.PayoutEndpointDSL ->
  ApiTokenInfo ->
  Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildPayoutManagementServerTransaction endpoint apiTokenInfo driverId =
  T.buildTransaction (DT.ProviderManagementAPI $ API.Types.ProviderPlatform.Management.PayoutAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) driverId Nothing

getPayoutPayoutReferralHistory :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.Payout.PayoutReferralHistoryRes
getPayoutPayoutReferralHistory merchantShortId opCity apiTokenInfo areActivatedRidesOnly customerPhoneNo from limit offset referredByDriver to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.payoutDSL.getPayoutPayoutReferralHistory) areActivatedRidesOnly customerPhoneNo from limit offset referredByDriver to

getPayoutPayoutHistory :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.Payout.PayoutHistoryRes
getPayoutPayoutHistory merchantShortId opCity apiTokenInfo driverId driverPhoneNo from isFailedOnly limit offset to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.payoutDSL.getPayoutPayoutHistory) driverId driverPhoneNo from isFailedOnly limit offset to

postPayoutPayoutVerifyFraudStatus :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.UpdateFraudStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postPayoutPayoutVerifyFraudStatus merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVerifyFraudStatusEndpoint apiTokenInfo (Just req.driverId) (Just req)
  T.withTransactionStoring transaction $ do
    ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.payoutDSL.postPayoutPayoutVerifyFraudStatus) req

postPayoutPayoutRetryFailed :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.FailedRetryPayoutReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryFailed merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVerifyFraudStatusEndpoint apiTokenInfo (Nothing) (Just req)
  T.withTransactionStoring transaction $ do
    ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetryFailed) req

postPayoutPayoutRetryAllWithStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Payout.RetryPayoutsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPayoutPayoutRetryAllWithStatus merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildPayoutManagementServerTransaction API.Types.ProviderPlatform.Management.Payout.PostPayoutPayoutVerifyFraudStatusEndpoint apiTokenInfo (Nothing) (Just req)
  T.withTransactionStoring transaction $ do
    ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.payoutDSL.postPayoutPayoutRetryAllWithStatus) req
