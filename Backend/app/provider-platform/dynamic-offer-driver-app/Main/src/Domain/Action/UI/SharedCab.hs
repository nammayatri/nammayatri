{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SharedCab where

-- Shared-cab driver-facing actions. PURE PASS-THROUGH: load driver + vehicle,
-- reject unless variant == SHARED_CAB (400, 04-driver-side-plan.md §4), then
-- forward to rider-app's /internal/sharedCab/* via SharedLogic.CallSharedCabBAP.
-- driverId comes from TokenAuth; vehicleNumber from the driver's Vehicle row;
-- integratedBppConfigId from the SHARED_CAB IntegratedBPPConfig of the driver's
-- merchant operating city; serviceTierType is SHARED_CAB; capacity defaults to
-- 4 when the Vehicle row has no explicit capacity.
-- Flag ownership (4.5 R11): selectSharedCabRoute sets DriverInformation
-- .sharedCabSessionActive True BEFORE the BAP call (fail-CLOSED set side) and
-- endSharedCabRoute clears it ONLY after a successful route END whose response
-- decodes to null. updateSharedCabSessionActive (the designated writer, also the
-- LTS choke point) does every write; the 4.2B reconciler is otherwise the only
-- clearer. Nothing else here writes session/flag/Redis/DB state.
-- (R26) both flag writes below go through SharedLogic.SharedCab.Flag
-- (set-/clearSharedCabSessionActive) — the single exported wrapper around
-- updateSharedCabSessionActive inside the cross-app master Redis cell; this
-- module no longer calls the query fn directly.

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
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.CallBAPInternal as SharedCabBAP
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified Storage.Queries.DriverInformationExtra as QDriverInformationExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type DriverAuthInfo = (Id SP.Person, Id Merchant, Id MerchantOperatingCity)

sharedCabDefaultCapacity :: Int
sharedCabDefaultCapacity = 4

sharedCabVehicleCategory :: Text
sharedCabVehicleCategory = "SHARED_CAB"

sharedCabServiceTierType :: Text
sharedCabServiceTierType = show DCommon.SHARED_CAB

-- Gate: caller must be a DRIVER whose current vehicle is a shared cab.
-- Returns the Vehicle row (registrationNo == vehicleNumber, capacity).
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
  SharedCabBAP.getSharedCabRoutes bap.apiKey bap.url integratedBPPConfig.id.getId lat lon

-- 4.5 R11: set the taxi-pool exclusion flag BEFORE the BAP route/select call is
-- issued (fail-CLOSED: if the call then fails, the driver stays excluded until the
-- 4.2B reconciler clears the flag — never the other way round). Idempotent on
-- driverId+vehicleNumber: a driver has at most one live shared-cab vehicle, the
-- DriverInformation row and its LTS shadow are keyed by driverId, and a
-- re-submitted select finds the flag already True and skips the write — no
-- double-set, no toggle trip.
setSharedCabSessionActiveBeforeSelect :: Id SP.Person -> Flow ()
setSharedCabSessionActiveBeforeSelect driverId = do
  mbDriverInfo <- QDriverInformationExtra.findById (cast driverId)
  case mbDriverInfo of
    Just driverInfo | driverInfo.sharedCabSessionActive -> pure ()
    _ -> SharedCabFlag.setSharedCabSessionActive driverId

selectSharedCabRoute :: DriverAuthInfo -> SelectRouteReq -> Flow SelectRouteResp
selectSharedCabRoute (personId, _merchantId, merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  setSharedCabSessionActiveBeforeSelect personId
  SharedCabBAP.selectSharedCabRoute bap.apiKey bap.url $
    SharedCabBAP.BAPSelectRouteReq
      { SharedCabBAP.mode = req.mode,
        SharedCabBAP.routeCode = req.routeCode,
        SharedCabBAP.walkupCount = req.walkupCount,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo,
        SharedCabBAP.integratedBppConfigId = integratedBPPConfig.id.getId,
        SharedCabBAP.serviceTierType = sharedCabServiceTierType,
        SharedCabBAP.capacity = fromMaybe sharedCabDefaultCapacity vehicle.capacity
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
  -- 4.5 R11: the flag clears ONLY after a successful route END whose response
  -- decodes to null (rider-app: next = END drops the session). A session payload
  -- (RETURN/CHANGE) keeps the driver excluded; any error above already threw and
  -- left the flag alone. The 4.2B reconciler is otherwise the only clearer.
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
getSharedCabTrips (personId, _merchantId, _merchantOpCityId) mbDate = do
  _ <- validateSharedCabDriver personId
  bap <- bapInternal
  -- rider-app's date query param is mandatory; an undated request means
  -- "today's runs" (4.5), and rider-app interprets the day in IST (UTC+5:30).
  now <- getCurrentTime
  let todayIST = utctDay (addUTCTime 19800 now)
  SharedCabBAP.getSharedCabTrips bap.apiKey bap.url personId.getId (show (fromMaybe todayIST mbDate))
