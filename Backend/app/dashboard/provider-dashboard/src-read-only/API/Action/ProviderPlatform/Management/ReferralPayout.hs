{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.ReferralPayout
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.ReferralPayout
import qualified Dashboard.Common
import qualified Data.Text
import qualified Domain.Action.ProviderPlatform.Management.ReferralPayout
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("referralPayout" :> (PostReferralPayoutDashboardPayoutDeleteVpa :<|> GetReferralPayoutDashboardPayoutRegistration :<|> GetReferralPayoutDashboardPayoutOrderStatus))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postReferralPayoutDashboardPayoutDeleteVpa merchantId city :<|> getReferralPayoutDashboardPayoutRegistration merchantId city :<|> getReferralPayoutDashboardPayoutOrderStatus merchantId city

type PostReferralPayoutDashboardPayoutDeleteVpa =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.REFERRAL_PAYOUT) / ('API.Types.ProviderPlatform.Management.ReferralPayout.POST_REFERRAL_PAYOUT_DASHBOARD_PAYOUT_DELETE_VPA))
      :> API.Types.ProviderPlatform.Management.ReferralPayout.PostReferralPayoutDashboardPayoutDeleteVpa
  )

type GetReferralPayoutDashboardPayoutRegistration =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.REFERRAL_PAYOUT) / ('API.Types.ProviderPlatform.Management.ReferralPayout.GET_REFERRAL_PAYOUT_DASHBOARD_PAYOUT_REGISTRATION))
      :> API.Types.ProviderPlatform.Management.ReferralPayout.GetReferralPayoutDashboardPayoutRegistration
  )

type GetReferralPayoutDashboardPayoutOrderStatus =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.REFERRAL_PAYOUT) / ('API.Types.ProviderPlatform.Management.ReferralPayout.GET_REFERRAL_PAYOUT_DASHBOARD_PAYOUT_ORDER_STATUS))
      :> API.Types.ProviderPlatform.Management.ReferralPayout.GetReferralPayoutDashboardPayoutOrderStatus
  )

postReferralPayoutDashboardPayoutDeleteVpa :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postReferralPayoutDashboardPayoutDeleteVpa merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ReferralPayout.postReferralPayoutDashboardPayoutDeleteVpa merchantShortId opCity apiTokenInfo driverId

getReferralPayoutDashboardPayoutRegistration :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ReferralPayout.ReferralPayoutRegistrationRes)
getReferralPayoutDashboardPayoutRegistration merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ReferralPayout.getReferralPayoutDashboardPayoutRegistration merchantShortId opCity apiTokenInfo driverId

getReferralPayoutDashboardPayoutOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver)) -> Data.Text.Text -> Environment.FlowHandler Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
getReferralPayoutDashboardPayoutOrderStatus merchantShortId opCity apiTokenInfo driverId orderId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ReferralPayout.getReferralPayoutDashboardPayoutOrderStatus merchantShortId opCity apiTokenInfo driverId orderId
