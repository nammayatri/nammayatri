{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AutoAcceptRide
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AutoAcceptRide
import qualified Control.Lens
import qualified Domain.Action.UI.AutoAcceptRide
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "driver" :> "autoAcceptRide" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking) :> "acknowledge"
      :> ReqBody
           ('[JSON])
           API.Types.UI.AutoAcceptRide.AutoAcceptRideReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postDriverAutoAcceptRideAcknowledge

postDriverAutoAcceptRideAcknowledge ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    API.Types.UI.AutoAcceptRide.AutoAcceptRideReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDriverAutoAcceptRideAcknowledge a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.AutoAcceptRide.postDriverAutoAcceptRideAcknowledge (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
