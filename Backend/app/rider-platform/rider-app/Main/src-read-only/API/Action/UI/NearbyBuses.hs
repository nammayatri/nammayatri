{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.NearbyBuses
  ( API,
    handler,
  )
where

import qualified API.Types.UI.NearbyBuses
import qualified BecknV2.FRFS.Enums
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.NearbyBuses
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "nearbyBusBooking" :> ReqBody ('[JSON]) API.Types.UI.NearbyBuses.NearbyBusesRequest
      :> Post
           ('[JSON])
           API.Types.UI.NearbyBuses.NearbyBusesResponse
      :<|> TokenAuth
      :> "nextVehicleDetails"
      :> Capture "routeCode" Data.Text.Text
      :> Capture
           "stopCode"
           Data.Text.Text
      :> QueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.JourneyModule.Utils.UpcomingTripInfo
      :<|> TokenAuth
      :> "timetable"
      :> Capture
           "routeCode"
           Data.Text.Text
      :> "stop"
      :> Capture
           "stopCode"
           Data.Text.Text
      :> QueryParam
           "toCode"
           Data.Text.Text
      :> QueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           ('[JSON])
           API.Types.UI.NearbyBuses.TimetableResponse
  )

handler :: Environment.FlowServer API
handler = postNearbyBusBooking :<|> getNextVehicleDetails :<|> getTimetableStop

postNearbyBusBooking ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyBuses.NearbyBusesRequest ->
    Environment.FlowHandler API.Types.UI.NearbyBuses.NearbyBusesResponse
  )
postNearbyBusBooking a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NearbyBuses.postNearbyBusBooking (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getNextVehicleDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Data.Text.Text ->
    Kernel.Prelude.Maybe (BecknV2.FRFS.Enums.VehicleCategory) ->
    Environment.FlowHandler Lib.JourneyModule.Utils.UpcomingTripInfo
  )
getNextVehicleDetails a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NearbyBuses.getNextVehicleDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getTimetableStop ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Data.Text.Text ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Kernel.Prelude.Maybe (BecknV2.FRFS.Enums.VehicleCategory) ->
    Environment.FlowHandler API.Types.UI.NearbyBuses.TimetableResponse
  )
getTimetableStop a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NearbyBuses.getTimetableStop (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1
