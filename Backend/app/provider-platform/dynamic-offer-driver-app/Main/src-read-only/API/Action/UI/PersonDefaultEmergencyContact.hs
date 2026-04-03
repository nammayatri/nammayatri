{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.PersonDefaultEmergencyContact 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.PersonDefaultEmergencyContact
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.PersonDefaultEmergencyContact
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "driver" :> "personDefaultEmergencyContacts" :> Get ('[JSON])
                                                                             [API.Types.UI.PersonDefaultEmergencyContact.PersonDefaultEmergencyContact] :<|> TokenAuth :> "driver" :> "personDefaultEmergencyContacts" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                  API.Types.UI.PersonDefaultEmergencyContact.UpdatePersonDefaultEmergencyContactsReq :> Put ('[JSON])
                                                                                                                                                                                                                                                                                                                            Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = getDriverPersonDefaultEmergencyContacts :<|> putDriverPersonDefaultEmergencyContacts
getDriverPersonDefaultEmergencyContacts :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                             Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                             Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler [API.Types.UI.PersonDefaultEmergencyContact.PersonDefaultEmergencyContact])
getDriverPersonDefaultEmergencyContacts a1 = withFlowHandlerAPI $ Domain.Action.UI.PersonDefaultEmergencyContact.getDriverPersonDefaultEmergencyContacts (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
putDriverPersonDefaultEmergencyContacts :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                             Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                             Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.PersonDefaultEmergencyContact.UpdatePersonDefaultEmergencyContactsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putDriverPersonDefaultEmergencyContacts a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PersonDefaultEmergencyContact.putDriverPersonDefaultEmergencyContacts (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



