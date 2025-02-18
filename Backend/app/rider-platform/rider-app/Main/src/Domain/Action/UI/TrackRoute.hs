module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import Data.Function
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import Tools.Error

getTrackVehicles ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow TrackRoute.TrackingResp
  )
getTrackVehicles (mbPersonId, merchantId) routeCode = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  vehicleTracking <- trackVehicles personId merchantId routeCode
  pure $ TrackRoute.TrackingResp {vehicleTrackingInfo = map mkVehicleTrackingResponse vehicleTracking}
  where
    mkVehicleTrackingResponse VehicleTracking {..} = TrackRoute.VehicleInfo {vehicleInfo = mkVehicleInfo vehicleInfo, ..}
    mkVehicleInfo VehicleInfo {..} = TrackRoute.VehicleInfoForRoute {..}
