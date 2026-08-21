module API.Internal.VehicleServiceTierList
  ( API,
    handler,
  )
where

import Domain.Action.Internal.VehicleServiceTierList (VehicleServiceTierInfo)
import qualified Domain.Action.Internal.VehicleServiceTierList as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> Capture "city" Context.City
    :> "vehicleServiceTiers"
    :> Header "token" Text
    :> Get '[JSON] [VehicleServiceTierInfo]

handler :: FlowServer API
handler = getVehicleServiceTiers

getVehicleServiceTiers ::
  Id Merchant ->
  Context.City ->
  Maybe Text ->
  FlowHandler [VehicleServiceTierInfo]
getVehicleServiceTiers merchantId city mbToken =
  withFlowHandlerAPI $ Domain.getVehicleServiceTiers merchantId city mbToken
