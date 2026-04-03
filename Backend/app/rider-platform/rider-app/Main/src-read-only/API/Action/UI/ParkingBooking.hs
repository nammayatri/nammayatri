{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.ParkingBooking 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.ParkingBooking
import qualified Kernel.Prelude
import qualified Environment
import qualified Data.Text
import qualified API.Types.UI.ParkingBooking



type API = ("multimodal" :> "parking" :> "book" :> Header "api-key" Data.Text.Text :> ReqBody ('[JSON]) API.Types.UI.ParkingBooking.ParkingBookingReq :> Post ('[JSON])
                                                                                                                                                              API.Types.UI.ParkingBooking.ParkingBookingResponse)
handler :: Environment.FlowServer API
handler = postMultimodalParkingBook
postMultimodalParkingBook :: (Kernel.Prelude.Maybe (Data.Text.Text) -> API.Types.UI.ParkingBooking.ParkingBookingReq -> Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingBookingResponse)
postMultimodalParkingBook a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ParkingBooking.postMultimodalParkingBook a2 a1



