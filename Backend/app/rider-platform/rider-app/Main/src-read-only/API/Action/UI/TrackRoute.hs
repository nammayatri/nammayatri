{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TrackRoute
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TrackRoute
import qualified BecknV2.FRFS.Enums
import qualified Control.Lens
import qualified Domain.Action.UI.TrackRoute as Domain.Action.UI.TrackRoute
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
  ( TokenAuth :> "track" :> Capture "routeCode" Kernel.Prelude.Text :> "vehicles" :> QueryParam "vehicleType" BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           API.Types.UI.TrackRoute.TrackingResp
  )

handler :: Environment.FlowServer API
handler = getTrackVehicles

getTrackVehicles ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler API.Types.UI.TrackRoute.TrackingResp
  )
getTrackVehicles a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TrackRoute.getTrackVehicles (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
