{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.DriverProfileQuestions 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.DriverProfileQuestions
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.DriverProfileQuestions
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "DriverProfileQues" :> ReqBody ('[JSON]) API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq :> Post ('[JSON])
                                                                                                                                   Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "DriverProfileQues" :> QueryParam "isImages" Kernel.Prelude.Bool :> Get ('[JSON])
                                                                                                                                                                                                                                                                API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes)
handler :: Environment.FlowServer API
handler = postDriverProfileQues :<|> getDriverProfileQues
postDriverProfileQues :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverProfileQues a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverProfileQuestions.postDriverProfileQues (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getDriverProfileQues :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                          Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                          Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes)
getDriverProfileQues a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverProfileQuestions.getDriverProfileQues (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



