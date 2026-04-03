{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.SpecialLocationWarrior 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.SpecialLocationWarrior
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Data.Text
import qualified Lib.Queries.SpecialLocation
import qualified API.Types.UI.SpecialLocationWarrior
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "specialLocation" :> "list" :> "category" :> MandatoryQueryParam "category" Data.Text.Text :> Get ('[JSON])
                                                                                                                           [Lib.Queries.SpecialLocation.SpecialLocationWarrior] :<|> TokenAuth :> "getInfo" :> "specialLocWarrior" :> MandatoryQueryParam "driverId"
                                                                                                                                                                                                                                                          (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                 API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes :<|> TokenAuth :> "updateInfo" :> "specialLocWarrior" :> MandatoryQueryParam "driverId"
                                                                                                                                                                                                                                                                                                                                                                                                                                                           (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes)
handler :: Environment.FlowServer API
handler = getSpecialLocationListCategory :<|> getGetInfoSpecialLocWarrior :<|> postUpdateInfoSpecialLocWarrior
getSpecialLocationListCategory :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Data.Text.Text -> Environment.FlowHandler [Lib.Queries.SpecialLocation.SpecialLocationWarrior])
getSpecialLocationListCategory a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialLocationWarrior.getSpecialLocationListCategory (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getGetInfoSpecialLocWarrior :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                 Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                 Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes)
getGetInfoSpecialLocWarrior a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialLocationWarrior.getGetInfoSpecialLocWarrior (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
postUpdateInfoSpecialLocWarrior :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                     Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                     Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoReq -> Environment.FlowHandler API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes)
postUpdateInfoSpecialLocWarrior a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialLocationWarrior.postUpdateInfoSpecialLocWarrior (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1



