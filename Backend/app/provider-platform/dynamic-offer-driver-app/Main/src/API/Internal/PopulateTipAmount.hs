module API.Internal.PopulateTipAmount
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.PopulateTipAmount as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  Capture "rideId" (Id Ride)
    :> Capture "tipAmount" HighPrecMoney
    :> "populateTipAmount"
    :> Header "token" Text
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  populateTipAmount

populateTipAmount :: Id Ride -> HighPrecMoney -> Maybe Text -> FlowHandler APISuccess
populateTipAmount rideId tipAmount = withFlowHandlerAPI . Domain.populateTipAmount rideId tipAmount
