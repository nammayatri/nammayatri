{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Dispatcher
  ( getDispatcherGetFleetInfo,
    postDispatcherUpdateFleetSchedule,
    getDispatcherDepotNames,
    getDispatcherDepotIds,
    getDispatcherGetVehiclesByDepotName,
    getDispatcherGetVehiclesByDepotId,
    getFleetOverrideInfo,
    delFleetOverrideInfo,
  )
where

import qualified API.Types.UI.Dispatcher
import qualified BecknV2.OnDemand.Enums as BecknSpec
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
  vehicleInfo <- OTPRest.getVehicleInfo integratedBPPConfig fleetId >>= fromMaybeM (DepotFleetInfoNotFound fleetId)
  unless depotManager.isAdmin $ do
    when (depotManager.depotCode.getId /= vehicleInfo.depotName) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId fleetId
  -- =========== validation done ===========
  pure $ API.Types.UI.Dispatcher.DispatcherRes {conductorCode = vehicleInfo.conductorCode, driverCode = vehicleInfo.driverCode, depotName = vehicleInfo.depotName, scheduleNo = vehicleInfo.scheduleNo}

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
  updatedFleetInfo <- OTPRest.getVehicleInfo integratedBPPConfig req.updatedFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.updatedFleetId)
  unless depotManager.isAdmin $ do
    sourceFleetInfo <- OTPRest.getVehicleInfo integratedBPPConfig req.sourceFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.sourceFleetId)
    when (depotManager.depotCode.getId /= sourceFleetInfo.depotName) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.sourceFleetId
    when (depotManager.depotCode.getId /= updatedFleetInfo.depotName) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.updatedFleetId
  -- =========== validation done ===========
  -- adding fleet override info in redis for waybill.
  Redis.setExp (fleetOverrideKey req.sourceFleetId) (req.updatedFleetId, updatedFleetInfo.waybillNo) 2400
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
    Environment.Flow [Kernel.Prelude.Int]
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
    Kernel.Prelude.Int ->
    Environment.Flow [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotId (mbPersonId, _merchantId) depotId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  nandiVehicles <- Flow.getVehiclesFromByDepotId baseUrl (Just depotId)
  pure $ map (\(NandiTypes.DepotVehicle {fleet_no = f, status = s, vehicle_no = v}) -> API.Types.UI.Dispatcher.DepotVehicle {fleet_no = f, status = s, vehicle_no = v}) nandiVehicles
