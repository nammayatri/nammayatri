{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCab
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCab
import qualified Control.Lens
import qualified Domain.Action.UI.SharedCab
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "sharedCab" :> "routes" :> Get ('[JSON]) API.Types.UI.SharedCab.SharedCabRouteListResp :<|> TokenAuth :> "sharedCab" :> "routes"
      :> Capture
           "routeCode"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabRouteDetailResp
      :<|> TokenAuth
      :> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "skip"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabSkipReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getSharedCabRoutes :<|> getSharedCabRoute :<|> postSharedCabBookingSkip

getSharedCabRoutes :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.SharedCab.SharedCabRouteListResp)
getSharedCabRoutes a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCab.getSharedCabRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getSharedCabRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.SharedCab.SharedCabRouteDetailResp
  )
getSharedCabRoute a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCab.getSharedCabRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSharedCabBookingSkip ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    API.Types.UI.SharedCab.SharedCabSkipReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSharedCabBookingSkip a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SharedCab.postSharedCabBookingSkip (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
