{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.EditLocation 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.EditLocation
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.BookingUpdateRequest
import qualified API.Types.UI.EditLocation
import qualified Kernel.Types.APISuccess



type API = (TokenAuth :> "edit" :> Capture "bookingUpdateRequestId" (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest) :> "result" :> Get ('[JSON])
                                                                                                                                                                   API.Types.UI.EditLocation.EditLocationResultAPIResp :<|> TokenAuth :> "edit" :> "result" :> Capture "bookingUpdateRequestId"
                                                                                                                                                                                                                                                                       (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest) :> "confirm" :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                        Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = getEditResult :<|> postEditResultConfirm
getEditResult :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                   Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> Environment.FlowHandler API.Types.UI.EditLocation.EditLocationResultAPIResp)
getEditResult a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EditLocation.getEditResult (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
postEditResultConfirm :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEditResultConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EditLocation.postEditResultConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



