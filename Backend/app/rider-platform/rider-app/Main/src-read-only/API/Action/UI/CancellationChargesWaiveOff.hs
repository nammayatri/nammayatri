{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CancellationChargesWaiveOff
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CancellationChargesWaiveOff
import qualified Control.Lens
import qualified Domain.Action.UI.CancellationChargesWaiveOff
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "rideBooking" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> "cancellationChargesWaiveOff"
      :> Post
           ('[JSON])
           API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes
  )

handler :: Environment.FlowServer API
handler = postRideBookingCancellationChargesWaiveOff

postRideBookingCancellationChargesWaiveOff ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Environment.FlowHandler API.Types.UI.CancellationChargesWaiveOff.CancellationChargesWaiveOffRes
  )
postRideBookingCancellationChargesWaiveOff a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CancellationChargesWaiveOff.postRideBookingCancellationChargesWaiveOff (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
