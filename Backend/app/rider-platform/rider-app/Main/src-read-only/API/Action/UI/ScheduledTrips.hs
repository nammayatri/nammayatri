{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ScheduledTrips
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.ScheduledTrips as Domain.Action.UI.ScheduledTrips
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "scheduled" :> Capture "routeId" (Kernel.Types.Id.Id Domain.Types.Route.Route) :> "trips" :> Get ('[JSON]) Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = getScheduledTrips

getScheduledTrips ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Route.Route ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
getScheduledTrips a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ScheduledTrips.getScheduledTrips (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
