{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SharedCabExtra where

-- Shared-cab driver-facing actions: a pass-through that rejects unless the variant is SHARED_CAB and forwards to rider-app.
-- selectSharedCabRoute sets sharedCabSessionActive True BEFORE the BAP call (fail-closed); endSharedCabRoute clears it only after a
-- successful route END. Both go through SharedLogic.SharedCab.Flag; the reconciler is otherwise the only clearer.

import API.Types.UI.SharedCab
import qualified Domain.Types.Common as DCommon
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as SP
import Domain.Types.SharedCabSession
import qualified Domain.Types.Vehicle as DVehicle
import qualified Domain.Types.VehicleVariant as DV
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator (SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified SharedLogic.SharedCab.ReconcilerSeed as SharedCabSeed
import qualified SharedLogic.SharedCab.RiderClient as SharedCabBAP
import qualified Storage.Queries.DriverInformationExtra as QDriverInformationExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type DriverAuthInfo = (Id SP.Person, Id Merchant, Id MerchantOperatingCity)

type TokenAuthInfo = (Maybe (Id SP.Person), Id Merchant, Id MerchantOperatingCity)

requireDriver :: TokenAuthInfo -> Flow DriverAuthInfo
requireDriver (mbPersonId, merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  pure (personId, merchantId, merchantOpCityId)

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

getSharedCabRouteData :: DriverAuthInfo -> Flow SharedCabRouteDataResp
getSharedCabRouteData (personId, _merchantId, merchantOpCityId) = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  SharedCabBAP.getSharedCabRouteData bap.apiKey bap.url integratedBPPConfig.agencyKey

postSharedCabRoutesDemand :: DriverAuthInfo -> SharedCabDemandReq -> Flow SharedCabDemandResp
postSharedCabRoutesDemand (personId, _merchantId, merchantOpCityId) req = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  SharedCabBAP.postSharedCabRoutesDemand bap.apiKey bap.url SharedCabBAP.BAPDemandReq {SharedCabBAP.agencyId = integratedBPPConfig.agencyKey, SharedCabBAP.routeCodes = req.routeCodes}

-- set the exclusion flag BEFORE the BAP call (fail-closed: a failed call leaves the driver excluded until the reconciler clears it)
setSharedCabSessionActiveBeforeSelect :: Id SP.Person -> Flow ()
setSharedCabSessionActiveBeforeSelect driverId = do
  mbDriverInfo <- QDriverInformationExtra.findById (cast driverId)
  case mbDriverInfo of
    Just driverInfo | driverInfo.sharedCabSessionActive -> pure ()
    _ -> SharedCabFlag.setSharedCabSessionActive driverId

routeOperation :: DriverAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
routeOperation auth req = case req.operation of
  SELECT_ROUTE -> do
    routeCode <- fromMaybeM (InvalidRequest "routeCode is required to select a route") req.routeCode
    walkupCount <- fromMaybeM (InvalidRequest "walkupCount is required to select a route") req.walkupCount
    selectSharedCabRoute auth req routeCode walkupCount
  _ -> endSharedCabRoute auth req

selectSharedCabRoute :: DriverAuthInfo -> SharedCabRouteOperationReq -> Text -> Int -> Flow SelectRouteResp
selectSharedCabRoute (personId, merchantId, merchantOpCityId) req routeCode walkupCount = do
  vehicle <- validateSharedCabDriver personId
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  setSharedCabSessionActiveBeforeSelect personId
  -- dead-chain recovery: a select is the city's hottest driver action, so re-run the idempotent chain seeder (a cheap no-op
  -- while the marker is alive). Fire-and-forget: a seed failure is logged, never propagated into the driver's select.
  SharedCabSeed.trySeedSharedCabReconcilerChain (Just merchantId) merchantOpCityId (SharedCabReconcilerJobData merchantId merchantOpCityId)
  SharedCabBAP.postRouteOperation bap.apiKey bap.url $
    SharedCabBAP.BAPRouteOperationReq
      { SharedCabBAP.operation = req.operation,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo,
        SharedCabBAP.agencyId = Just integratedBPPConfig.agencyKey,
        SharedCabBAP.serviceTierType = Just sharedCabServiceTierType,
        SharedCabBAP.capacity = Just (fromMaybe sharedCabDefaultCapacity vehicle.capacity),
        SharedCabBAP.routeCode = Just routeCode,
        SharedCabBAP.walkupCount = Just walkupCount,
        SharedCabBAP.mode = req.mode,
        SharedCabBAP.driverName = Just (unwords (catMaybes [Just driver.firstName, driver.lastName])),
        SharedCabBAP.vehicleModel = Just vehicle.model,
        SharedCabBAP.atLastStop = Nothing,
        SharedCabBAP.forceEnd = Nothing
      }

getSharedCabSession :: DriverAuthInfo -> Flow (Maybe SharedCabSession)
getSharedCabSession (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.getSharedCabSession bap.apiKey bap.url personId.getId vehicle.registrationNo

endSharedCabRoute :: DriverAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
endSharedCabRoute (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  resp <-
    SharedCabBAP.postRouteOperation bap.apiKey bap.url $
      SharedCabBAP.BAPRouteOperationReq
        { SharedCabBAP.operation = req.operation,
          SharedCabBAP.driverId = personId.getId,
          SharedCabBAP.vehicleNumber = vehicle.registrationNo,
          SharedCabBAP.agencyId = Nothing,
          SharedCabBAP.serviceTierType = Nothing,
          SharedCabBAP.capacity = Nothing,
          SharedCabBAP.routeCode = Nothing,
          SharedCabBAP.walkupCount = Nothing,
          SharedCabBAP.mode = Nothing,
          SharedCabBAP.driverName = Nothing,
          SharedCabBAP.vehicleModel = Nothing,
          SharedCabBAP.atLastStop = req.atLastStop,
          SharedCabBAP.forceEnd = req.forceEnd
        }
  -- the flag clears ONLY after a successful route END whose response carries no session; a session payload (RETURN/CHANGE) keeps
  -- the driver excluded
  when (isNothing resp.session) $
    SharedCabFlag.clearSharedCabSessionActive personId
  pure resp

sessionOperation :: DriverAuthInfo -> SharedCabSessionOperationReq -> Flow SharedCabSession
sessionOperation (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.postSessionOperation bap.apiKey bap.url $
    SharedCabBAP.BAPSessionOperationReq
      { SharedCabBAP.operation = req.operation,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo,
        SharedCabBAP.version = req.version,
        SharedCabBAP.walkupCount = req.walkupCount
      }
