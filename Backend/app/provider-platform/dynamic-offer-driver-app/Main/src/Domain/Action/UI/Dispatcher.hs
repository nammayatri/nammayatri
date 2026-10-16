module Domain.Action.UI.Dispatcher
  ( getDispatcherGetFleetInfo,
    postDispatcherUpdateFleetSchedule,
    getDispatcherDepots,
    getDispatcherGetVehiclesByDepotId,
    getDispatcherGetUserDepot,
    getDispatcherHistory,
  )
where

import qualified API.Types.UI.Dispatcher
import qualified BecknV2.FRFS.Enums as BecknSpec
import qualified Domain.Types.DepotManager as DDM
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleActionHistory as DVAH
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.DepotManagerExtra as QDMExtra
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleActionHistory as QVAH
import Tools.Auth ()
import Tools.Error

-- | Dispatcher-only: a checker has no depot row, and ACCESS_DENIED is the code the app maps for
-- one reaching these endpoints. Admin scope is read live off the row per request.
requireDepotManager ::
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Environment.Flow (DP.Person, DDM.DepotManager)
requireDepotManager mbPersonId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role == DP.PT_PROVIDER) $ throwError AccessDenied
  -- Prefers the admin row like operator login does, so a multi-depot manager is not scoped
  -- against an arbitrary depot.
  depotManager <- B.runInReplica $ QDMExtra.findBestForOperatorByPersonId personId >>= fromMaybeM (DepotManagerNotFound personId.getId)
  unless depotManager.enabled $ throwError AccessDenied
  -- Dispatch needs a depot to act on, so a block-only operator is refused here.
  unless (Kernel.Prelude.isJust depotManager.depotCode || depotManager.isAdmin) $ throwError AccessDenied
  pure (person, depotManager)

getDispatcherGetFleetInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.Dispatcher.DispatcherRes
  )
getDispatcherGetFleetInfo (mbPersonId, _merchantId, _merchantOpCityId) fleetId = do
  (person, depotManager) <- requireDepotManager mbPersonId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  vehicleInfo <- OTPRest.getFleetOperationInfo integratedBPPConfig fleetId >>= fromMaybeM (DepotFleetInfoNotFound fleetId)
  unless depotManager.isAdmin $
    when (depotManager.depotCode /= Just vehicleInfo.depot_id) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId fleetId
  pure $
    API.Types.UI.Dispatcher.DispatcherRes
      { conductorCode = fromMaybe "" vehicleInfo.conductor_code,
        driverCode = fromMaybe "" vehicleInfo.driver_code,
        depotName = fromMaybe "" vehicleInfo.depot_name,
        scheduleNo = fromMaybe "" vehicleInfo.schedule_no
      }

postDispatcherUpdateFleetSchedule ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Dispatcher.DispatcherReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDispatcherUpdateFleetSchedule (mbPersonId, _merchantId, _merchantOpCityId) req = do
  (person, depotManager) <- requireDepotManager mbPersonId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  sourceFleetInfo <- OTPRest.getFleetOperationInfo integratedBPPConfig req.sourceFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.sourceFleetId)
  unless depotManager.isAdmin $ do
    updatedFleetInfo <- OTPRest.getFleetOperationInfo integratedBPPConfig req.updatedFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.updatedFleetId)
    when (depotManager.depotCode /= Just sourceFleetInfo.depot_id) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.sourceFleetId
    when (depotManager.depotCode /= Just updatedFleetInfo.depot_id) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.updatedFleetId
  -- An empty waybill makes the consumer classify the override as finished and delete it, so reject up front.
  sourceWaybillNo <-
    sourceFleetInfo.waybill_no
      & fromMaybeM (InvalidRequest $ "No active waybill for source fleet " <> req.sourceFleetId <> "; its schedule cannot be overridden")
  now <- getCurrentTime
  historyId <- generateGUID
  let (reasonTag, reasonContent) = case req.reason of
        API.Types.UI.Dispatcher.BreakDown -> ("BreakDown", Nothing)
        API.Types.UI.Dispatcher.OtherReason txt -> ("OtherReason", Just txt)
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  let vehicleActionHistory =
        DVAH.VehicleActionHistory
          { id = historyId,
            dispatcherId = personId,
            action = DVAH.DISPATCHER,
            currentVehicle = req.sourceFleetId,
            replacedVehicle = Just req.updatedFleetId,
            driverCode = sourceFleetInfo.driver_code,
            conductorCode = sourceFleetInfo.conductor_code,
            merchantId = person.merchantId,
            merchantOperatingCityId = person.merchantOperatingCityId,
            depotId = depotManager.depotCode,
            reasonTag = reasonTag,
            reasonContent = reasonContent,
            createdAt = now,
            updatedAt = now,
            waybillNo = sourceFleetInfo.waybill_no
          }
  QVAH.create vehicleActionHistory
  Redis.withCrossAppRedis $ Redis.setExp (fleetOverrideKey req.updatedFleetId) (req.sourceFleetId, sourceWaybillNo) 86400
  pure Kernel.Types.APISuccess.Success

-- Cross-app key: the consumer is rider-app, which can't address our Hedis prefix.
fleetOverrideKey :: Text -> Text
fleetOverrideKey sourceFleetId = "shared:fleetOverride:sourceFleetId:" <> sourceFleetId

getDispatcherDepots ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotOption]
  )
getDispatcherDepots (mbPersonId, _merchantId, _merchantOpCityId) = do
  (person, _) <- requireDepotManager mbPersonId
  integratedBPPConfig <- getBusIbppConfig person.merchantOperatingCityId
  depots <- OTPRest.getDepots integratedBPPConfig
  pure $ map toApiDepotOption depots

toApiDepotOption :: NandiTypes.DepotInfo -> API.Types.UI.Dispatcher.DepotOption
toApiDepotOption depot = API.Types.UI.Dispatcher.DepotOption {depotId = depot.depot_id, depotName = depot.depot_name}

getDispatcherGetVehiclesByDepotId ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotId (mbPersonId, _merchantId, _merchantOpCityId) depotId = do
  (person, depotManager) <- requireDepotManager mbPersonId
  unless depotManager.isAdmin $
    when (depotManager.depotCode /= Just depotId) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId depotId
  integratedBPPConfig <- getBusIbppConfig person.merchantOperatingCityId
  nandiVehicles <- OTPRest.getDepotVehicles integratedBPPConfig depotId Kernel.Prelude.Nothing
  pure $ map toApiDepotVehicle nandiVehicles

toApiDepotVehicle :: NandiTypes.DepotVehicleInfo -> API.Types.UI.Dispatcher.DepotVehicle
toApiDepotVehicle NandiTypes.DepotVehicleInfo {fleet_no, status, vehicle_no, service_type} =
  API.Types.UI.Dispatcher.DepotVehicle {fleet_no, status, vehicle_no, service_type}

getDispatcherGetUserDepot ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.Dispatcher.UserDepotRes
  )
getDispatcherGetUserDepot (mbPersonId, _merchantId, _merchantOpCityId) = do
  (person, depotManager) <- requireDepotManager mbPersonId
  depotCode <- depotManager.depotCode & fromMaybeM (InvalidRequest "Depot manager has no depot assigned")
  -- GIMS owns depot names; the code is what callers act on, so a GIMS blip must not fail bootstrap.
  eDepot <-
    withTryCatch "getUserDepot:getDepotById" $ do
      integratedBPPConfig <- getBusIbppConfig person.merchantOperatingCityId
      OTPRest.getDepotById integratedBPPConfig depotCode
  pure $ API.Types.UI.Dispatcher.UserDepotRes {depotName = either (const Kernel.Prelude.Nothing) (fmap (.depot_name)) eDepot, depot = Just depotCode}

getDispatcherHistory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow [API.Types.UI.Dispatcher.DispatcherHistoryRes]
  )
getDispatcherHistory (mbPersonId, _merchantId, _merchantOpCityId) mbLimit mbOffset = do
  (_, _) <- requireDepotManager mbPersonId
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  let limit = min 100 (max 1 (fromMaybe 15 mbLimit))
      offset = fromMaybe 0 mbOffset
  historyRecords <- B.runInReplica $ QVAH.findAllByDispatcherIdAndAction (Just limit) (Just offset) personId DVAH.DISPATCHER
  pure $ map convertToHistoryRes historyRecords
  where
    convertToHistoryRes :: DVAH.VehicleActionHistory -> API.Types.UI.Dispatcher.DispatcherHistoryRes
    convertToHistoryRes DVAH.VehicleActionHistory {..} =
      API.Types.UI.Dispatcher.DispatcherHistoryRes
        { API.Types.UI.Dispatcher.id = Kernel.Types.Id.getId id,
          API.Types.UI.Dispatcher.dispatcherId = Kernel.Types.Id.getId dispatcherId,
          API.Types.UI.Dispatcher.currentVehicle = currentVehicle,
          API.Types.UI.Dispatcher.replacedVehicle = fromMaybe "" replacedVehicle,
          API.Types.UI.Dispatcher.historyDriverCode = fromMaybe "" driverCode,
          API.Types.UI.Dispatcher.historyConductorCode = fromMaybe "" conductorCode,
          API.Types.UI.Dispatcher.depotId = fromMaybe "" depotId,
          API.Types.UI.Dispatcher.reasonTag = reasonTag,
          API.Types.UI.Dispatcher.reasonContent = reasonContent,
          API.Types.UI.Dispatcher.createdAt = createdAt,
          API.Types.UI.Dispatcher.updatedAt = updatedAt,
          API.Types.UI.Dispatcher.waybillNo = fromMaybe "" waybillNo
        }

getBusIbppConfig ::
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Environment.Flow DIBC.IntegratedBPPConfig
getBusIbppConfig merchantOpCityId = SIBC.findIntegratedBPPConfig Nothing merchantOpCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
