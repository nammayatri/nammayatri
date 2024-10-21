{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ScheduledTrips
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ScheduledTrips
import qualified Control.Lens
import qualified Domain.Action.UI.ScheduledTrips as Domain.Action.UI.ScheduledTrips
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "scheduled" :> Capture "routeId" (Kernel.Types.Id.Id Domain.Types.Route.Route) :> "trips" :> ReqBody ('[JSON]) API.Types.UI.ScheduledTrips.CurrLocation
      :> Post
           ('[JSON])
           API.Types.UI.ScheduledTrips.TrackingResp
  )

handler :: Environment.FlowServer API
handler = postScheduledTrips

postScheduledTrips ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Route.Route ->
    API.Types.UI.ScheduledTrips.CurrLocation ->
    Environment.FlowHandler API.Types.UI.ScheduledTrips.TrackingResp
  )
postScheduledTrips a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ScheduledTrips.postScheduledTrips (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
