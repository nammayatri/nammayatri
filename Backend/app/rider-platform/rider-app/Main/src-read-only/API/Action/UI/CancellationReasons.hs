{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CancellationReasons
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CancellationReasons
import qualified Control.Lens
import qualified Domain.Action.UI.CancellationReasons
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "rideBooking" :> Capture "rideBookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> "cancellationReasons"
      :> Header
           "x-language"
           Kernel.External.Types.Language
      :> Get ('[JSON]) [API.Types.UI.CancellationReasons.CancellationReasonEntity]
  )

handler :: Environment.FlowServer API
handler = getRideBookingCancellationReasons

getRideBookingCancellationReasons ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Environment.FlowHandler [API.Types.UI.CancellationReasons.CancellationReasonEntity]
  )
getRideBookingCancellationReasons a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CancellationReasons.getRideBookingCancellationReasons (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
