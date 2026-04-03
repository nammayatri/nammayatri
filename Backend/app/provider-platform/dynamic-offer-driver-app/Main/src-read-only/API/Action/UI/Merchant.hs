{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Merchant 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Merchant
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.Merchant
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "cityConfigs" :> Get ('[JSON]) API.Types.UI.Merchant.CityConfigs)
handler :: Environment.FlowServer API
handler = getCityConfigs
getCityConfigs :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler API.Types.UI.Merchant.CityConfigs)
getCityConfigs a1 = withFlowHandlerAPI $ Domain.Action.UI.Merchant.getCityConfigs (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)



