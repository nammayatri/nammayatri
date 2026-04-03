{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.DriverSafetySettings 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.DriverSafetySettings
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.DriverSafetySettings
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "driver" :> "getSafetySettings" :> Capture "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get ('[JSON])
                                                                                                                                      API.Types.UI.DriverSafetySettings.GetDriverSafetySettingsRes :<|> TokenAuth :> "driver" :> "updateSafetySettings" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                   API.Types.UI.DriverSafetySettings.UpdateDriverSafetySettingsReq :> Put ('[JSON])
                                                                                                                                                                                                                                                                                                                                          Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = getDriverGetSafetySettings :<|> putDriverUpdateSafetySettings
getDriverGetSafetySettings :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.UI.DriverSafetySettings.GetDriverSafetySettingsRes)
getDriverGetSafetySettings a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverSafetySettings.getDriverGetSafetySettings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
putDriverUpdateSafetySettings :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                   Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                   Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.DriverSafetySettings.UpdateDriverSafetySettingsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putDriverUpdateSafetySettings a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverSafetySettings.putDriverUpdateSafetySettings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



