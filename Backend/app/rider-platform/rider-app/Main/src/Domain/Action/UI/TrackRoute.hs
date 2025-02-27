module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import qualified BecknV2.FRFS.Enums as Spec
import Data.Function
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import Storage.Queries.Person as QP
import Tools.Error

getTrackVehicles ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Spec.VehicleCategory ->
    Environment.Flow TrackRoute.TrackingResp
  )
getTrackVehicles (mbPersonId, merchantId) routeCode mbVehicleType = do
  let vehicleType = fromMaybe Spec.BUS mbVehicleType
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  personCityInfo <- QP.findCityInfoById personId >>= fromMaybeM (PersonNotFound personId.getId)
  vehicleTracking <- trackVehicles personId merchantId personCityInfo.merchantOperatingCityId vehicleType routeCode
  pure $ TrackRoute.TrackingResp {vehicleTrackingInfo = map mkVehicleTrackingResponse vehicleTracking}
  where
    mkVehicleTrackingResponse VehicleTracking {..} = TrackRoute.VehicleInfo {vehicleInfo = mkVehicleInfo vehicleInfo, ..}
    mkVehicleInfo VehicleInfo {..} = TrackRoute.VehicleInfoForRoute {..}
