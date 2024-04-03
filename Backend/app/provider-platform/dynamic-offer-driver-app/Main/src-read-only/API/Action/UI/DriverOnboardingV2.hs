{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import qualified Control.Lens
import qualified Domain.Action.UI.DriverOnboardingV2 as Domain.Action.UI.DriverOnboardingV2
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "onboarding" :> "configs" :> Get ('[JSON]) API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList)

handler :: Environment.FlowServer API
handler = getOnboardingConfigs

getOnboardingConfigs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getOnboardingConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
