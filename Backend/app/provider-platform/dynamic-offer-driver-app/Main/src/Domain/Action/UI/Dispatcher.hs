module Domain.Action.UI.Dispatcher
  ( getDispatcherGetFleetInfo,
    postDispatcherUpdateFleetSchedule,
    getDispatcherDepotNames,
    getDispatcherDepotIds,
    getDispatcherGetVehiclesByDepotName,
    getDispatcherGetVehiclesByDepotId,
    getDispatcherGetDepotNameById,
    getDispatcherGetUserDepot,
    getDispatcherHistory,
    getFleetOverrideInfo,
    delFleetOverrideInfo,
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
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import qualified Lib.GtfsDataServer.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Depot as QDepot
import qualified Storage.Queries.DepotManager as QDM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleActionHistory as QVAH
import Tools.Auth ()
import Tools.Error

-- | Resolve caller → (Person, DepotManager) and enforce role gate. `allowedRoles`
-- must include BUS_DISPATCHER for admin ops, and both roles for read-only ops.
requireDepotManagerAs ::
  [DP.Role] ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Environment.Flow (DP.Person, DDM.DepotManager)
requireDepotManagerAs allowedRoles mbPersonId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role `elem` allowedRoles) $ throwError AccessDenied
  depotManager <- B.runInReplica $ QDM.findByPersonId personId >>= fromMaybeM (DepotManagerNotFound personId.getId)
  pure (person, depotManager)

dispatcherRoles :: [DP.Role]
dispatcherRoles = [DP.BUS_DISPATCHER]

anyDepotManagerRoles :: [DP.Role]
anyDepotManagerRoles = [DP.BUS_CHECKER, DP.BUS_DISPATCHER]

getDispatcherGetFleetInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.Dispatcher.DispatcherRes
  )
getDispatcherGetFleetInfo (mbPersonId, _merchantId, _merchantOpCityId) fleetId = do
  (person, depotManager) <- requireDepotManagerAs dispatcherRoles mbPersonId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  vehicleInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig fleetId >>= fromMaybeM (DepotFleetInfoNotFound fleetId)
  unless depotManager.isAdmin $
    when (depotManager.depotCode.getId /= vehicleInfo.depot_id) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId fleetId
  pure $
    API.Types.UI.Dispatcher.DispatcherRes
      { conductorCode = fromMaybe "" vehicleInfo.conductor_code,
        driverCode = fromMaybe "" vehicleInfo.driver_code,
        depotName = vehicleInfo.depot_name,
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
  (person, depotManager) <- requireDepotManagerAs dispatcherRoles mbPersonId
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  sourceFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.sourceFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.sourceFleetId)
  unless depotManager.isAdmin $ do
    updatedFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.updatedFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.updatedFleetId)
    when (depotManager.depotCode.getId /= sourceFleetInfo.depot_id) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.sourceFleetId
    when (depotManager.depotCode.getId /= updatedFleetInfo.depot_id) $
      throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.updatedFleetId
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
            depotId = Just depotManager.depotCode.getId,
            reasonTag = reasonTag,
            reasonContent = reasonContent,
            createdAt = now,
            updatedAt = now,
            waybillNo = sourceFleetInfo.waybill_no
          }
  QVAH.create vehicleActionHistory
  Redis.setExp (fleetOverrideKey req.updatedFleetId) (req.sourceFleetId, fromMaybe "" sourceFleetInfo.waybill_no) 86400
  pure Kernel.Types.APISuccess.Success

getFleetOverrideInfo :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m (Maybe (Text, Text))
getFleetOverrideInfo sourceFleetId = Redis.safeGet (fleetOverrideKey sourceFleetId)

delFleetOverrideInfo :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m ()
delFleetOverrideInfo sourceFleetId = Redis.del (fleetOverrideKey sourceFleetId)

fleetOverrideKey :: Text -> Text
fleetOverrideKey sourceFleetId = "fleetOverride:sourceFleetId:" <> sourceFleetId

getDispatcherDepotNames ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow [Kernel.Prelude.Text]
  )
getDispatcherDepotNames (mbPersonId, _merchantId, _merchantOpCityId) = do
  (person, _) <- requireDepotManagerAs dispatcherRoles mbPersonId
  baseUrl <- getNandiBaseUrl person.merchantOperatingCityId
  NandiFlow.getDepotNames baseUrl

getDispatcherDepotIds ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow [Kernel.Prelude.Text]
  )
getDispatcherDepotIds (mbPersonId, _merchantId, _merchantOpCityId) = do
  (person, _) <- requireDepotManagerAs dispatcherRoles mbPersonId
  baseUrl <- getNandiBaseUrl person.merchantOperatingCityId
  NandiFlow.getDepotIds baseUrl

getDispatcherGetVehiclesByDepotName ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotName (mbPersonId, _merchantId, _merchantOpCityId) depotName = do
  (person, _) <- requireDepotManagerAs dispatcherRoles mbPersonId
  baseUrl <- getNandiBaseUrl person.merchantOperatingCityId
  nandiVehicles <- NandiFlow.getVehiclesFromByDepotName baseUrl (Just depotName)
  pure $ map toApiDepotVehicle nandiVehicles

getDispatcherGetVehiclesByDepotId ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotId (mbPersonId, _merchantId, _merchantOpCityId) depotId = do
  (person, _) <- requireDepotManagerAs dispatcherRoles mbPersonId
  baseUrl <- getNandiBaseUrl person.merchantOperatingCityId
  nandiVehicles <- NandiFlow.getVehiclesFromByDepotId baseUrl (Just depotId)
  pure $ map toApiDepotVehicle nandiVehicles

toApiDepotVehicle :: NandiTypes.DepotVehicle -> API.Types.UI.Dispatcher.DepotVehicle
toApiDepotVehicle NandiTypes.DepotVehicle {fleet_no, status, vehicle_no} =
  API.Types.UI.Dispatcher.DepotVehicle {fleet_no, status, vehicle_no}

getDispatcherGetDepotNameById ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow Kernel.Prelude.Text
  )
getDispatcherGetDepotNameById (mbPersonId, _merchantId, _merchantOpCityId) depotId = do
  (person, _) <- requireDepotManagerAs anyDepotManagerRoles mbPersonId
  baseUrl <- getNandiBaseUrl person.merchantOperatingCityId
  NandiFlow.getDepotNameById baseUrl depotId

getDispatcherGetUserDepot ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.Dispatcher.UserDepotRes
  )
getDispatcherGetUserDepot (mbPersonId, _merchantId, _merchantOpCityId) = do
  (_person, depotManager) <- requireDepotManagerAs anyDepotManagerRoles mbPersonId
  let depotCode = depotManager.depotCode.getId
  -- Local Depot table is the source of truth for the human name of a depot id we manage.
  mbDepotName <- fmap (.name) <$> QDepot.findByPrimaryKey depotManager.depotCode
  pure $ API.Types.UI.Dispatcher.UserDepotRes {depotName = mbDepotName, depot = Just depotCode}

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
  (_, _) <- requireDepotManagerAs dispatcherRoles mbPersonId
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  let limit = fromMaybe 15 mbLimit
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

getNandiBaseUrl ::
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Environment.Flow BaseUrl
getNandiBaseUrl merchantOpCityId = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing merchantOpCityId (show BecknSpec.BUS) DIBC.MULTIMODAL
  SIBC.getGimsBaseUrl integratedBPPConfig
