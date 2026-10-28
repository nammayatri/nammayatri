{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SharedCab where

-- Shared-cab driver-facing actions: a pass-through that rejects unless the variant is SHARED_CAB and forwards to rider-app.
-- selectSharedCabRoute sets sharedCabSessionActive True BEFORE the BAP call (fail-closed); endSharedCabRoute clears it only after a
-- successful route END. Both go through SharedLogic.SharedCab.Flag; the reconciler is otherwise the only clearer.

import API.Types.UI.SharedCab
import Data.Time (utctDay)
import Data.Time.Calendar (Day)
import qualified Domain.Types.Common as DCommon
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as DVehicle
import qualified Domain.Types.VehicleVariant as DV
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Allocator (SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified SharedLogic.SharedCab.ReconcilerSeed as SharedCabSeed
import qualified SharedLogic.SharedCab.RiderClient as SharedCabBAP
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDriverInformationExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type DriverAuthInfo = (Id SP.Person, Id Merchant, Id MerchantOperatingCity)

sharedCabDefaultCapacity :: Int
sharedCabDefaultCapacity = 6

sharedCabVehicleCategory :: Text
sharedCabVehicleCategory = "SHARED_CAB"

sharedCabServiceTierType :: Text
sharedCabServiceTierType = show DCommon.SHARED_CAB

validateSharedCabDriver :: Id SP.Person -> Flow DVehicle.Vehicle
validateSharedCabDriver personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  unless (driver.role == SP.DRIVER) $ throwError (InvalidRequest ("Person " <> personId.getId <> " is not a driver"))
  vehicle <- QVehicle.findById personId >>= fromMaybeM (DriverWithoutVehicle personId.getId)
  unless (vehicle.variant == DV.SHARED_CAB) $
    throwError $
      InvalidRequest ("Vehicle variant " <> show vehicle.variant <> " is not a shared cab")
  pure vehicle

sharedCabBPPConfig :: Id MerchantOperatingCity -> Flow DIBC.IntegratedBPPConfig
sharedCabBPPConfig merchantOpCityId =
  findIntegratedBPPConfig Nothing merchantOpCityId sharedCabVehicleCategory APPLICATION

bapInternal :: Flow AppBackendBapInternal
bapInternal = asks (.appBackendBapInternal)

getSharedCabRoutes :: DriverAuthInfo -> Double -> Double -> Flow SharedCabRoutesResp
getSharedCabRoutes (personId, _merchantId, merchantOpCityId) lat lon = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  SharedCabBAP.getSharedCabRoutes bap.apiKey bap.url integratedBPPConfig.agencyKey lat lon

-- set the exclusion flag BEFORE the BAP call (fail-closed: a failed call leaves the driver excluded until the reconciler clears it)
setSharedCabSessionActiveBeforeSelect :: Id SP.Person -> Flow ()
setSharedCabSessionActiveBeforeSelect driverId = do
  mbDriverInfo <- QDriverInformationExtra.findById (cast driverId)
  case mbDriverInfo of
    Just driverInfo | driverInfo.sharedCabSessionActive -> pure ()
    _ -> SharedCabFlag.setSharedCabSessionActive driverId

selectSharedCabRoute :: DriverAuthInfo -> SelectRouteReq -> Flow SelectRouteResp
selectSharedCabRoute (personId, merchantId, merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  setSharedCabSessionActiveBeforeSelect personId
  -- dead-chain recovery: a select is the city's hottest driver action, so re-run the idempotent chain seeder (a cheap no-op
  -- while the marker is alive). Fire-and-forget: a seed failure is logged, never propagated into the driver's select.
  SharedCabSeed.trySeedSharedCabReconcilerChain (Just merchantId) merchantOpCityId (SharedCabReconcilerJobData merchantId merchantOpCityId)
  SharedCabBAP.selectSharedCabRoute bap.apiKey bap.url $
    SharedCabBAP.BAPSelectRouteReq
      { SharedCabBAP.mode = req.mode,
        SharedCabBAP.routeCode = req.routeCode,
        SharedCabBAP.walkupCount = req.walkupCount,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo,
        SharedCabBAP.agencyId = integratedBPPConfig.agencyKey,
        SharedCabBAP.serviceTierType = sharedCabServiceTierType,
        SharedCabBAP.capacity = fromMaybe sharedCabDefaultCapacity vehicle.capacity,
        SharedCabBAP.driverName = Just (unwords (catMaybes [Just driver.firstName, driver.lastName])),
        SharedCabBAP.vehicleModel = Just vehicle.model
      }

getSharedCabSession :: DriverAuthInfo -> Flow (Maybe SharedCabSession)
getSharedCabSession (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.getSharedCabSession bap.apiKey bap.url personId.getId vehicle.registrationNo

setSharedCabSeats :: DriverAuthInfo -> SeatsReq -> Flow SharedCabSession
setSharedCabSeats (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.setSharedCabSeats bap.apiKey bap.url $
    SharedCabBAP.BAPSeatsReq
      { SharedCabBAP.version = req.version,
        SharedCabBAP.walkupCount = req.walkupCount,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo
      }

endSharedCabRoute :: DriverAuthInfo -> EndRouteReq -> Flow (Maybe SharedCabSession)
endSharedCabRoute (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  mbSession <-
    SharedCabBAP.endSharedCabRoute bap.apiKey bap.url $
      SharedCabBAP.BAPEndRouteReq
        { SharedCabBAP.next = req.next,
          SharedCabBAP.atLastStop = req.atLastStop,
          SharedCabBAP.force = req.force,
          SharedCabBAP.driverId = personId.getId,
          SharedCabBAP.vehicleNumber = vehicle.registrationNo
        }
  -- the flag clears ONLY after a successful route END whose response decodes to null; a session payload (RETURN/CHANGE) keeps
  -- the driver excluded
  when (isNothing mbSession) $
    SharedCabFlag.clearSharedCabSessionActive personId
  pure mbSession

resumeSharedCab :: DriverAuthInfo -> Flow SharedCabSession
resumeSharedCab (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.resumeSharedCab bap.apiKey bap.url $
    SharedCabBAP.BAPResumeReq
      { SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo
      }

getSharedCabTrips :: DriverAuthInfo -> Maybe Day -> Flow SharedCabTripsResp
getSharedCabTrips (personId, _merchantId, merchantOpCityId) mbDate = do
  _ <- validateSharedCabDriver personId
  bap <- bapInternal
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  today <- utctDay <$> getLocalCurrentTime transporterConfig.timeDiffFromUtc
  SharedCabBAP.getSharedCabTrips bap.apiKey bap.url personId.getId (show (fromMaybe today mbDate))
