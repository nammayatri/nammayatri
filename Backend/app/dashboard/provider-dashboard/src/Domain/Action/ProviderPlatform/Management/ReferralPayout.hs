{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.ReferralPayout
  ( postReferralPayoutDashboardPayoutDeleteVpa,
    getReferralPayoutDashboardPayoutRegistration,
    getReferralPayoutDashboardPayoutOrderStatus,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.ReferralPayout
import qualified "dashboard-helper-api" Dashboard.Common
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

-- Driver id from token when UI omits optional query; pass to BPP via same query param.
postReferralPayoutDashboardPayoutDeleteVpa :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postReferralPayoutDashboardPayoutDeleteVpa merchantShortId opCity apiTokenInfo mbDriverId = do
  let driverId = Kernel.Prelude.fromMaybe (Kernel.Types.Id.cast apiTokenInfo.personId) (mbDriverId >>= id)
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) (Kernel.Prelude.Just driverId) Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.referralPayoutDSL.postReferralPayoutDashboardPayoutDeleteVpa) (Kernel.Prelude.Just driverId)

getReferralPayoutDashboardPayoutRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Environment.Flow API.Types.ProviderPlatform.Management.ReferralPayout.ReferralPayoutRegistrationRes)
getReferralPayoutDashboardPayoutRegistration merchantShortId opCity apiTokenInfo mbDriverId = do
  let driverId = Kernel.Prelude.fromMaybe (Kernel.Types.Id.cast apiTokenInfo.personId) (mbDriverId >>= id)
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.referralPayoutDSL.getReferralPayoutDashboardPayoutRegistration) (Kernel.Prelude.Just driverId)

getReferralPayoutDashboardPayoutOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Data.Text.Text -> Environment.Flow Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
getReferralPayoutDashboardPayoutOrderStatus merchantShortId opCity apiTokenInfo mbDriverId orderId = do
  let driverId = Kernel.Prelude.fromMaybe (Kernel.Types.Id.cast apiTokenInfo.personId) (mbDriverId >>= id)
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.referralPayoutDSL.getReferralPayoutDashboardPayoutOrderStatus) (Kernel.Prelude.Just driverId) orderId
