{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Fleet.Onboarding
  ( API.Types.ProviderPlatform.Fleet.Onboarding.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified API.Types.ProviderPlatform.Management.Endpoints.Account
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Fleet.Onboarding
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleCategory
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Fleet.Onboarding.API)
handler merchantId city = getOnboardingDocumentConfigs merchantId city :<|> getOnboardingRegisterStatus merchantId city :<|> postOnboardingVerify merchantId city :<|> getOnboardingGetReferralDetails merchantId city

getOnboardingDocumentConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Onboarding.Role -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList)
getOnboardingDocumentConfigs a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingDocumentConfigs a6 a5 a4 a3 a2 a1

getOnboardingRegisterStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.StatusRes)
getOnboardingRegisterStatus a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingRegisterStatus a8 a7 a6 a5 a4 a3 a2 a1

postOnboardingVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Endpoints.Account.DashboardAccessType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.VerifyDocumentRes)
postOnboardingVerify a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.postOnboardingVerify a6 a5 a4 a3 a2 a1

getOnboardingGetReferralDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.ReferralInfoRes)
getOnboardingGetReferralDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingGetReferralDetails a4 a3 a2 a1
