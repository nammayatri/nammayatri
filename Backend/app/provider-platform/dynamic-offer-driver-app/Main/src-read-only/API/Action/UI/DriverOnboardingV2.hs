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
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "onboarding" :> "configs" :> QueryParam "onlyVehicle" Kernel.Prelude.Bool
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      :<|> TokenAuth
      :> "driver"
      :> "updateAirCondition"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "vehicleServiceTiers"
      :> Get
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      :<|> TokenAuth
      :> "driver"
      :> "updateServiceTiers"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "driver"
      :> "register"
      :> "ssn"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverOnboardingV2.SSNReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getOnboardingConfigs :<|> postDriverUpdateAirCondition :<|> getDriverVehicleServiceTiers :<|> postDriverUpdateServiceTiers :<|> postDriverRegisterSsn

getOnboardingConfigs ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getOnboardingConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverUpdateAirCondition ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.UpdateAirConditionUpdateRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateAirCondition a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverUpdateAirCondition (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDriverVehicleServiceTiers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
  )
getDriverVehicleServiceTiers a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.getDriverVehicleServiceTiers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postDriverUpdateServiceTiers ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverUpdateServiceTiers a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverUpdateServiceTiers (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDriverRegisterSsn ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.SSNReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverRegisterSsn a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverOnboardingV2.postDriverRegisterSsn (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
