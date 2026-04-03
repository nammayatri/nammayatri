{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.EditBooking 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.EditBooking
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.BookingUpdateRequest
import qualified API.Types.UI.EditBooking
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "edit" :> "result" :> Capture "bookingUpdateRequestId" (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest) :> ReqBody ('[JSON])
                                                                                                                                                                       API.Types.UI.EditBooking.EditBookingRespondAPIReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postEditResult
postEditResult :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> API.Types.UI.EditBooking.EditBookingRespondAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEditResult a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EditBooking.postEditResult (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1



