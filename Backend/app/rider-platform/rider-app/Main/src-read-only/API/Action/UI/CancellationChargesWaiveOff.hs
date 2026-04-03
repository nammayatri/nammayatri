{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.CancellationChargesWaiveOff 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.CancellationChargesWaiveOff
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.Booking
import qualified API.Types.UI.CancellationChargesWaiveOff



type API = (TokenAuth :> "rideBooking" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> "cancellationChargesWaiveOff" :> Post ('[JSON])
                                                                                                                                                         API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes)
handler :: Environment.FlowServer API
handler = postRideBookingCancellationChargesWaiveOff
postRideBookingCancellationChargesWaiveOff :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                                Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Environment.FlowHandler API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes)
postRideBookingCancellationChargesWaiveOff a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CancellationChargesWaiveOff.postRideBookingCancellationChargesWaiveOff (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



