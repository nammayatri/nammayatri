{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.NearbyBuses
  ( API,
    handler,
  )
where

import qualified API.Types.UI.NearbyBuses
import qualified Control.Lens
import qualified Domain.Action.UI.NearbyBuses as Domain.Action.UI.NearbyBuses
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

type API = (TokenAuth :> "nearbyBusBooking" :> ReqBody ('[JSON]) API.Types.UI.NearbyBuses.NearbyBusesRequest :> Post ('[JSON]) API.Types.UI.NearbyBuses.NearbyBusesResponse)

handler :: Environment.FlowServer API
handler = postNearbyBusBooking

postNearbyBusBooking ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyBuses.NearbyBusesRequest ->
    Environment.FlowHandler API.Types.UI.NearbyBuses.NearbyBusesResponse
  )
postNearbyBusBooking a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NearbyBuses.postNearbyBusBooking (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
