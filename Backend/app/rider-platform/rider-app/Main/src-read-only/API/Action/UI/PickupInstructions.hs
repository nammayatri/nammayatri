{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.PickupInstructions
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.PickupInstructions
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.PickupInstructions
import qualified Kernel.Types.APISuccess
import qualified Kernel.ServantMultipart



type API = (TokenAuth :> "pickupinstructions" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp API.Types.UI.PickupInstructions.PickupInstructionsReq :> Post ('[JSON])
                                                                                                                                                                                 Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "pickupinstructions" :> "closest" :> QueryParam "lat"
                                                                                                                                                                                                                                                                                      Kernel.Prelude.Double :> QueryParam "lon"
                                                                                                                                                                                                                                                                                                                          Kernel.Prelude.Double :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                                                       API.Types.UI.PickupInstructions.ClosestPickupInstructionResp :<|> TokenAuth :> "pickupinstructions" :> QueryParam "lat"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Kernel.Prelude.Double :> QueryParam "lon"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Kernel.Prelude.Double :> QueryParam "target"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 API.Types.UI.PickupInstructions.DeleteTarget :> Delete ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postPickupinstructions :<|> getPickupinstructionsClosest :<|> deletePickupinstructions
postPickupinstructions :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                            Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.PickupInstructions.PickupInstructionsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPickupinstructions a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.postPickupinstructions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getPickupinstructionsClosest :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Environment.FlowHandler API.Types.UI.PickupInstructions.ClosestPickupInstructionResp)
getPickupinstructionsClosest a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.getPickupinstructionsClosest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
deletePickupinstructions :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                              Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (API.Types.UI.PickupInstructions.DeleteTarget) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deletePickupinstructions a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.deletePickupinstructions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1



