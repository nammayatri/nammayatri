module API.Internal.Ride
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.Ride as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "ride"
      :> Capture "rideId" (Id Ride)
      :> "deliveryImage"
      :> Header "token" Text
      :> Get '[JSON] Text
  )

handler :: FlowServer API
handler =
  getDeliveryImage

getDeliveryImage :: Id Ride -> Maybe Text -> FlowHandler Text
getDeliveryImage rideId = withFlowHandlerAPI . Domain.getDeliveryImage rideId
