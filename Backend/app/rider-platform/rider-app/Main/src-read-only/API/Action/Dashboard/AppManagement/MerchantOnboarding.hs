{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.MerchantOnboarding
  ( API.Types.Dashboard.AppManagement.MerchantOnboarding.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Domain.Action.Dashboard.AppManagement.MerchantOnboarding as Domain.Action.Dashboard.AppManagement.MerchantOnboarding
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.MerchantOnboarding.API)
handler merchantId city = merchantOnboardingInfo merchantId city :<|> merchantOnboardingStart merchantId city :<|> merchantOnboardingList merchantId city

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingInfo a4 a3 a2 a1

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingStart a4 a3 a2 a1

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.MerchantOnboarding.merchantOnboardingList a3 a2 a1
