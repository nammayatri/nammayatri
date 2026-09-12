{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DriverReferral
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified Domain.Action.Dashboard.Management.DriverReferral
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("driverReferral" :> (PostDriverReferralReferralOpsPassword :<|> PostDriverReferralLinkReferral))

type PostDriverReferralReferralOpsPassword =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD"
      :> API.Types.ProviderPlatform.Management.DriverReferral.PostDriverReferralReferralOpsPassword
  )

type PostDriverReferralLinkReferral =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL"
      :> API.Types.ProviderPlatform.Management.DriverReferral.PostDriverReferralLinkReferral
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverReferralReferralOpsPassword merchantId city :<|> postDriverReferralLinkReferral merchantId city

postDriverReferralReferralOpsPassword :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverReferralReferralOpsPassword a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverReferral.postDriverReferralReferralOpsPassword a4 a3 a1

postDriverReferralLinkReferral :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverReferral.ReferralLinkReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverReferral.LinkReport)
postDriverReferralLinkReferral a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverReferral.postDriverReferralLinkReferral a4 a3 a1
