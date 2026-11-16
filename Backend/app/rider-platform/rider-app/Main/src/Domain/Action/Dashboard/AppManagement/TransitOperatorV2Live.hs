module Domain.Action.Dashboard.AppManagement.TransitOperatorV2Live
  ( transitOperatorV2ListLiveVehicles,
    transitOperatorV2GetLiveVehicle,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2Live
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.TransitLive as TransitLive
import SharedLogic.TransitOperatorV2 (transitFeedCtx)

transitOperatorV2ListLiveVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow API.Types.Dashboard.AppManagement.TransitOperatorV2Live.LiveVehicleList)
transitOperatorV2ListLiveVehicles merchantShortId opCity depotId routeId lat lon radiusMeters requestorId vehicleCategory = do
  feed <- transitFeedCtx merchantShortId opCity vehicleCategory requestorId
  TransitLive.listLiveVehicles feed depotId routeId lat lon radiusMeters

transitOperatorV2GetLiveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow API.Types.Dashboard.AppManagement.TransitOperatorV2Live.LiveVehicleDetail)
transitOperatorV2GetLiveVehicle merchantShortId opCity vehicleNo requestorId vehicleCategory = do
  feed <- transitFeedCtx merchantShortId opCity vehicleCategory requestorId
  TransitLive.getLiveVehicle feed vehicleNo
