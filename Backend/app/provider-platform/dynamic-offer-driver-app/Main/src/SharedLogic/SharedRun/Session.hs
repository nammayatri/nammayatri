{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | A shared-route run as the shared cab's session. The run is a ride; the rider platform keys its passengers by the
-- ride id and answers the ordinary session read with them, so the driver's shared-cab screens and verbs, the skip-stop
-- rule and the run's fare all read the one shape the shared cab already has.
module SharedLogic.SharedRun.Session
  ( RunSessionFlow,
    activeRunOf,
    runSession,
    stopCodeOf,
    boardingAt,
    alightingAt,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types as DTrip
import qualified Domain.Types.IntegratedBPPConfig as Fields
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import Domain.Types.SharedCabSession
import qualified Domain.Types.Vehicle as Fields
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.CallBAPInternal as Fields
import qualified SharedLogic.SharedCab.RiderClient as RiderClient
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type RunSessionFlow m r =
  ( MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasRequestId r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal]
  )

-- | The driver's shared-route run in progress, if any; a shared-cab driver on a plain ride has none.
activeRunOf :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => Id SP.Person -> m (Maybe DRide.Ride)
activeRunOf driverId = do
  mbRide <- QRide.getInProgressByDriverId driverId
  pure $ case mbRide of
    Just ride | DTrip.isSharedRouteTrip ride.tripCategory -> Just ride
    _ -> Nothing

-- | The run's session from the rider platform: the bookings placed on this ride, in the shared cab's session shape.
runSession :: RunSessionFlow m r => DRide.Ride -> m SharedCabSession
runSession ride = do
  vehicle <- QVehicle.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  integratedBppConfigId <- ride.integratedBppConfigId & fromMaybeM (InternalError $ "run " <> ride.id.getId <> " names no feed config")
  integratedBppConfig <- QIBC.findById (Id integratedBppConfigId) >>= fromMaybeM (InternalError $ "feed config " <> integratedBppConfigId <> " of run " <> ride.id.getId <> " not found")
  bap <- asks (.appBackendBapInternal)
  RiderClient.getSharedCabSession bap.apiKey bap.url ride.driverId.getId vehicle.registrationNo integratedBppConfig.agencyKey (Just ride.id.getId)
    >>= fromMaybeM (InternalError $ "the rider platform has no session for run " <> ride.id.getId)

-- | A run's stop is a Location; its GTFS stop code rides in the address's @extras@ (the only free text field), which is
-- how the session's stop codes are matched to the ride's stops.
stopCodeOf :: DLoc.Location -> Maybe Text
stopCodeOf location = location.address.extras

-- | Someone on the session still boards at this stop.
boardingAt :: Text -> SharedCabSession -> Bool
boardingAt code session = any (\r -> r.riderStatus /= BOARDED && r.boardStopCode == code) session.riders

-- | Someone aboard alights at this stop.
alightingAt :: Text -> SharedCabSession -> Bool
alightingAt code session = any (\r -> r.riderStatus == BOARDED && r.dropStopCode == code) session.riders
