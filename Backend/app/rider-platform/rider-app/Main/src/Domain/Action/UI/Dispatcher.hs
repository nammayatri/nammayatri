{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Dispatcher
  ( getDispatcherGetFleetInfo,
    postDispatcherUpdateFleetSchedule,
    getDispatcherDepotNames,
    getDispatcherDepotIds,
    getDispatcherGetVehiclesByDepotName,
    getDispatcherGetVehiclesByDepotId,
    getDispatcherGetDepotNameById,
    getDispatcherHistory,
    getDispatcherHistoryByDepotCode,
    getFleetOverrideInfo,
    delFleetOverrideInfo,
  )
where

import qualified API.Types.UI.Dispatcher
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Domain.Types.DispatcherHistory
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as Flow
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.Queries.DepotManager as QD
import qualified Storage.Queries.DispatcherHistory as QDH
import Storage.Queries.Person as QP
import Tools.Auth ()
import Tools.Error
import qualified Tools.MultiModal as MM

getDispatcherGetFleetInfo ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.Dispatcher.DispatcherRes
  )
getDispatcherGetFleetInfo (mbPersonId, _merchantId) fleetId = do
  -- validating if user has access to get fleet info
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  depotManager <- B.runInReplica $ QD.findByPersonId personId >>= fromMaybeM (DepotManagerNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  vehicleInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig fleetId >>= fromMaybeM (DepotFleetInfoNotFound fleetId)
  unless depotManager.isAdmin $ do
    when (depotManager.depotCode.getId /= vehicleInfo.depot_id) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId fleetId
  -- =========== validation done ===========
  pure $ API.Types.UI.Dispatcher.DispatcherRes {conductorCode = fromMaybe "" vehicleInfo.conductor_code, driverCode = fromMaybe "" vehicleInfo.driver_code, depotName = vehicleInfo.depot_name, scheduleNo = fromMaybe "" vehicleInfo.schedule_no}

postDispatcherUpdateFleetSchedule ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Dispatcher.DispatcherReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDispatcherUpdateFleetSchedule (mbPersonId, _merchantId) req = do
  -- validating if user has access to do updates to depot fleet schedules
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  depotManager <- B.runInReplica $ QD.findByPersonId personId >>= fromMaybeM (DepotManagerNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  sourceFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.sourceFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.sourceFleetId)
  unless depotManager.isAdmin $ do
    updatedFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.updatedFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.updatedFleetId)
    when (depotManager.depotCode.getId /= sourceFleetInfo.depot_id) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.sourceFleetId
    when (depotManager.depotCode.getId /= updatedFleetInfo.depot_id) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.updatedFleetId
  -- =========== validation done ===========
  -- Record history
  now <- getCurrentTime
  historyId <- generateGUID
  let (reasonTag, reasonContent) = case req.reason of
        API.Types.UI.Dispatcher.BreakDown -> ("BreakDown", Nothing)
        API.Types.UI.Dispatcher.OtherReason txt -> ("OtherReason", Just txt)
  let dispatcherHistory =
        Domain.Types.DispatcherHistory.DispatcherHistory
          { id = historyId,
            dispatcherId = personId,
            currentVehicle = req.sourceFleetId,
            replacedVehicle = req.updatedFleetId,
            driverCode = sourceFleetInfo.driver_code,
            conductorCode = sourceFleetInfo.conductor_code,
            merchantId = person.merchantId,
            merchantOperatingCityId = person.merchantOperatingCityId,
            depotId = depotManager.depotCode.getId,
            reasonTag = reasonTag,
            reasonContent = reasonContent,
            createdAt = now,
            updatedAt = now,
            waybillNo = sourceFleetInfo.waybill_no
          }
  QDH.create dispatcherHistory
  -- adding fleet override info in redis for waybill.
  Redis.setExp (fleetOverrideKey req.updatedFleetId) (req.sourceFleetId, sourceFleetInfo.waybill_no & fromMaybe "") 86400
  pure $ Kernel.Types.APISuccess.Success

getFleetOverrideInfo :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m (Maybe (Text, Text))
getFleetOverrideInfo sourceFleetId = Redis.safeGet (fleetOverrideKey sourceFleetId)

delFleetOverrideInfo :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m ()
delFleetOverrideInfo sourceFleetId = Redis.del (fleetOverrideKey sourceFleetId)

fleetOverrideKey :: Text -> Text
fleetOverrideKey sourceFleetId = "fleetOverride:sourceFleetId:" <> sourceFleetId

getDispatcherDepotNames ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow [Kernel.Prelude.Text]
  )
getDispatcherDepotNames (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getDepotNames baseUrl

getDispatcherDepotIds ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow [Kernel.Prelude.Text]
  )
getDispatcherDepotIds (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getDepotIds baseUrl

getDispatcherGetVehiclesByDepotName ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotName (mbPersonId, _merchantId) depotName = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  nandiVehicles <- Flow.getVehiclesFromByDepotName baseUrl (Just depotName)
  pure $ map (\(NandiTypes.DepotVehicle {fleet_no = f, status = s, vehicle_no = v}) -> API.Types.UI.Dispatcher.DepotVehicle {fleet_no = f, status = s, vehicle_no = v}) nandiVehicles

getDispatcherGetVehiclesByDepotId ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotId (mbPersonId, _merchantId) depotId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  nandiVehicles <- Flow.getVehiclesFromByDepotId baseUrl (Just depotId)
  pure $ map (\(NandiTypes.DepotVehicle {fleet_no = f, status = s, vehicle_no = v}) -> API.Types.UI.Dispatcher.DepotVehicle {fleet_no = f, status = s, vehicle_no = v}) nandiVehicles

getDispatcherGetDepotNameById ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow Kernel.Prelude.Text
  )
getDispatcherGetDepotNameById (mbPersonId, _merchantId) depotId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getDepotNameById baseUrl depotId

getDispatcherHistory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow [API.Types.UI.Dispatcher.DispatcherHistoryRes]
  )
getDispatcherHistory (mbPersonId, _merchantId) mbLimit mbOffset = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _ <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let limit = fromMaybe 15 mbLimit
      offset = fromMaybe 0 mbOffset
  historyRecords <- B.runInReplica $ QDH.findByDispatcherId (Just limit) (Just offset) personId
  pure $ map convertToHistoryRes historyRecords
  where
    convertToHistoryRes :: Domain.Types.DispatcherHistory.DispatcherHistory -> API.Types.UI.Dispatcher.DispatcherHistoryRes
    convertToHistoryRes Domain.Types.DispatcherHistory.DispatcherHistory {..} =
      API.Types.UI.Dispatcher.DispatcherHistoryRes
        { API.Types.UI.Dispatcher.id = Kernel.Types.Id.getId id,
          API.Types.UI.Dispatcher.dispatcherId = Kernel.Types.Id.getId dispatcherId,
          API.Types.UI.Dispatcher.currentVehicle = currentVehicle,
          API.Types.UI.Dispatcher.replacedVehicle = replacedVehicle,
          API.Types.UI.Dispatcher.historyDriverCode = fromMaybe "" driverCode,
          API.Types.UI.Dispatcher.historyConductorCode = fromMaybe "" conductorCode,
          API.Types.UI.Dispatcher.depotId = depotId,
          API.Types.UI.Dispatcher.reasonTag = reasonTag,
          API.Types.UI.Dispatcher.reasonContent = reasonContent,
          API.Types.UI.Dispatcher.createdAt = createdAt,
          API.Types.UI.Dispatcher.updatedAt = updatedAt,
          API.Types.UI.Dispatcher.waybillNo = fromMaybe "" waybillNo
        }

-- | Get dispatcher history by depotCode
getHistoryByDepotCode ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow [API.Types.UI.Dispatcher.DispatcherHistoryRes]
  )
getHistoryByDepotCode (mbPersonId, _merchantId) depotCode mbLimit mbOffset = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _ <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let limit = fromMaybe 15 mbLimit
      offset = fromMaybe 0 mbOffset
  historyRecords <- B.runInReplica $ QDH.findByDepotId (Just limit) (Just offset) depotCode
  pure $ map convertToHistoryRes historyRecords
  where
    convertToHistoryRes :: Domain.Types.DispatcherHistory.DispatcherHistory -> API.Types.UI.Dispatcher.DispatcherHistoryRes
    convertToHistoryRes Domain.Types.DispatcherHistory.DispatcherHistory {..} =
      API.Types.UI.Dispatcher.DispatcherHistoryRes
        { API.Types.UI.Dispatcher.id = Kernel.Types.Id.getId id,
          API.Types.UI.Dispatcher.dispatcherId = Kernel.Types.Id.getId dispatcherId,
          API.Types.UI.Dispatcher.currentVehicle = currentVehicle,
          API.Types.UI.Dispatcher.replacedVehicle = replacedVehicle,
          API.Types.UI.Dispatcher.historyDriverCode = fromMaybe "" driverCode,
          API.Types.UI.Dispatcher.historyConductorCode = fromMaybe "" conductorCode,
          API.Types.UI.Dispatcher.depotId = depotId,
          API.Types.UI.Dispatcher.reasonTag = reasonTag,
          API.Types.UI.Dispatcher.reasonContent = reasonContent,
          API.Types.UI.Dispatcher.createdAt = createdAt,
          API.Types.UI.Dispatcher.updatedAt = updatedAt,
          API.Types.UI.Dispatcher.waybillNo = fromMaybe "" waybillNo
        }

-- | API wrapper used by the HTTP handler; accepts optional depotCode query param
getDispatcherHistoryByDepotCode ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow [API.Types.UI.Dispatcher.DispatcherHistoryRes]
  )
getDispatcherHistoryByDepotCode (mbPersonId, _merchantId) mbDepotCode mbLimit mbOffset = do
  case mbDepotCode of
    Nothing -> pure []
    Just depotCode -> getHistoryByDepotCode (mbPersonId, _merchantId) depotCode mbLimit mbOffset
