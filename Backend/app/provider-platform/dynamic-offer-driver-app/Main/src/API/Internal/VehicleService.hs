module API.Internal.VehicleService
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.VehicleService as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "merchantCity" Context.City
    :> "serviceTierWithAssets"
    :> Header "token" Text
    :> Post '[JSON] Domain.VehicleServiceResp

handler :: FlowServer API
handler =
  vehicleServiceHandler

vehicleServiceHandler :: Id Merchant -> Context.City -> FlowHandler Domain.VehicleServiceResp
vehicleServiceHandler merchantId merchantCity = withFlowHandlerAPI . Domain.getVehicleService merchantId merchantCity
