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
    getFleetOverrideInfo,
    delFleetOverrideInfo,
    FleetOverrideScope (..),
    FleetScheduleChangeReq (..),
    postFleetSyncScheduleChange,
  )
where

import qualified API.Types.UI.Dispatcher
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Data.Aeson (withObject, (.:), (.:?))
import qualified Data.Time as Time
import qualified Domain.Types.DepotManager as Fields
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleActionHistory
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified SharedLogic.External.Nandi.Flow as Flow
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import Storage.Queries.DepotManager as QD
import Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleActionHistory as QVAH
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
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
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
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  sourceFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.sourceFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.sourceFleetId)
  unless depotManager.isAdmin $ do
    updatedFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.updatedFleetId >>= fromMaybeM (DepotFleetInfoNotFound req.updatedFleetId)
    when (depotManager.depotCode.getId /= sourceFleetInfo.depot_id) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.sourceFleetId
    when (depotManager.depotCode.getId /= updatedFleetInfo.depot_id) $ throwError $ DepotManagerDoesNotHaveAccessToFleet depotManager.personId.getId req.updatedFleetId
  sourceWaybillNo <-
    sourceFleetInfo.waybill_no
      & fromMaybeM (InvalidRequest $ "No active waybill for source fleet " <> req.sourceFleetId <> "; its schedule cannot be overridden")
  -- =========== validation done ===========
  -- Record history
  now <- getCurrentTime
  historyId <- generateGUID
  let (reasonTag, reasonContent) = case req.reason of
        API.Types.UI.Dispatcher.BreakDown -> ("BreakDown", Nothing)
        API.Types.UI.Dispatcher.OtherReason txt -> ("OtherReason", Just txt)
  let vehicleActionHistory =
        Domain.Types.VehicleActionHistory.VehicleActionHistory
          { id = historyId,
            dispatcherId = personId,
            action = Domain.Types.VehicleActionHistory.DISPATCHER,
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
  -- adding fleet override info in redis for waybill.
  Redis.setExp (fleetOverrideKey req.updatedFleetId) (req.sourceFleetId, sourceWaybillNo) 86400
  pure $ Kernel.Types.APISuccess.Success

data FleetScheduleChangeReq = FleetScheduleChangeReq
  { logsheetDate :: Time.Day,
    schNo :: Text,
    oldFleetNumber :: Text,
    newFleetNumber :: Text,
    driverCode :: Maybe Text,
    conductorCode :: Maybe Text
  }
  deriving (Generic, Show, Kernel.Prelude.ToSchema)

instance FromJSON FleetScheduleChangeReq where
  parseJSON = withObject "FleetScheduleChangeReq" $ \o ->
    FleetScheduleChangeReq
      <$> o .: "logsheet_date"
      <*> o .: "sch_no"
      <*> o .: "old_fleet_number"
      <*> (o .: "New_fleet_number" <|> o .: "new_fleet_number")
      <*> o .:? "driver_code"
      <*> o .:? "conductor_code"

postFleetSyncScheduleChange :: (DPO.PartnerOrganization, DPOC.FleetSyncConfig) -> FleetScheduleChangeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetSyncScheduleChange (partnerOrg, fleetSyncCfg) req = do
  rateLimitCfg <- CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.RATE_LIMIT >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.RATE_LIMIT) >>= DPOC.getRateLimitConfig . (.config)
  checkSlidingWindowLimitWithOptions ("BAP:FleetSync:PartnerOrgId:" <> partnerOrg.orgId.getId <> ":hitsCount") rateLimitCfg.rateLimitOptions
  let merchantOperatingCityId = Kernel.Types.Id.Id fleetSyncCfg.merchantOperatingCityId
  merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound fleetSyncCfg.merchantOperatingCityId)
  unless (merchantOperatingCity.merchantId == partnerOrg.merchantId) $ throwError (InvalidRequest "Partner organization is not configured for this city")
  now <- getCurrentTime
  let today = Time.utctDay (Time.addUTCTime 19800 now)
  unless (req.logsheetDate == today) $ throwError (InvalidRequest $ "logsheet_date must be today (" <> show today <> ")")
  when (req.oldFleetNumber == req.newFleetNumber) $ throwError (InvalidRequest "old_fleet_number and New_fleet_number are the same")
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
  newFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.newFleetNumber >>= fromMaybeM (DepotFleetInfoNotFound req.newFleetNumber)
  if newFleetInfo.schedule_no == Just req.schNo && isJust newFleetInfo.waybill_no
    then do
      logInfo $ "FleetSync: " <> req.newFleetNumber <> " already runs schedule " <> req.schNo <> " in GIMS, clearing overrides that still point at " <> req.oldFleetNumber
      sharedOverride :: Maybe (Text, Text) <- Redis.withCrossAppRedis $ Redis.safeGet (sharedFleetOverrideKey req.newFleetNumber)
      when ((fst <$> sharedOverride) == Just req.oldFleetNumber) $ Redis.withCrossAppRedis $ Redis.del (sharedFleetOverrideKey req.newFleetNumber)
      legacyOverride :: Maybe (Text, Text) <- Redis.safeGet (fleetOverrideKey req.newFleetNumber)
      when ((fst <$> legacyOverride) == Just req.oldFleetNumber) $ Redis.del (fleetOverrideKey req.newFleetNumber)
    else do
      oldFleetInfo <- OTPRest.getVehicleOperationInfo integratedBPPConfig req.oldFleetNumber >>= fromMaybeM (DepotFleetInfoNotFound req.oldFleetNumber)
      unless (oldFleetInfo.schedule_no == Just req.schNo) $
        throwError (InvalidRequest $ "Fleet " <> req.oldFleetNumber <> " is not running schedule " <> req.schNo)
      oldWaybillNo <- oldFleetInfo.waybill_no & fromMaybeM (InvalidRequest $ "No active waybill for fleet " <> req.oldFleetNumber)
      existing :: Maybe (Text, Text) <- Redis.withCrossAppRedis $ Redis.safeGet (sharedFleetOverrideKey req.newFleetNumber)
      unless (existing == Just (req.oldFleetNumber, oldWaybillNo)) $ do
        historyId <- generateGUID
        QVAH.create
          Domain.Types.VehicleActionHistory.VehicleActionHistory
            { id = historyId,
              dispatcherId = Kernel.Types.Id.cast partnerOrg.orgId,
              action = Domain.Types.VehicleActionHistory.FLEET_SYNC,
              currentVehicle = req.oldFleetNumber,
              replacedVehicle = Just req.newFleetNumber,
              driverCode = req.driverCode,
              conductorCode = req.conductorCode,
              merchantId = merchantOperatingCity.merchantId,
              merchantOperatingCityId = merchantOperatingCityId,
              depotId = Just oldFleetInfo.depot_id,
              reasonTag = "FleetSync",
              reasonContent = Just req.schNo,
              createdAt = now,
              updatedAt = now,
              waybillNo = Just oldWaybillNo
            }
        Redis.withCrossAppRedis $ Redis.setExp (sharedFleetOverrideKey req.newFleetNumber) (req.oldFleetNumber, oldWaybillNo) 86400
  pure Kernel.Types.APISuccess.Success

-- | Which key an override was read from. While both apps' dispatchers are live the two keys can
-- hold overrides for the same fleet, so the origin has to travel with the value: deleting both
-- would let whichever one was read as Finished wipe the other app's newer override.
data FleetOverrideScope = SharedFleetOverride | LegacyFleetOverride
  deriving (Show, Eq)

-- Dual-read, shared key first: driver-app's override is the authoritative one during cutover.
getFleetOverrideInfo :: (MonadFlow m, Redis.HedisFlow m r) => Text -> m (Maybe (FleetOverrideScope, (Text, Text)))
getFleetOverrideInfo sourceFleetId =
  Redis.withCrossAppRedis (Redis.safeGet (sharedFleetOverrideKey sourceFleetId)) >>= \case
    Just info -> pure (Just (SharedFleetOverride, info))
    Nothing -> fmap ((,) LegacyFleetOverride) <$> Redis.safeGet (fleetOverrideKey sourceFleetId)

delFleetOverrideInfo :: (MonadFlow m, Redis.HedisFlow m r) => FleetOverrideScope -> Text -> m ()
delFleetOverrideInfo scope sourceFleetId = case scope of
  SharedFleetOverride -> Redis.withCrossAppRedis $ Redis.del (sharedFleetOverrideKey sourceFleetId)
  LegacyFleetOverride -> Redis.del (fleetOverrideKey sourceFleetId)

fleetOverrideKey :: Text -> Text
fleetOverrideKey sourceFleetId = "fleetOverride:sourceFleetId:" <> sourceFleetId

-- Cross-app key written by driver-app, which can't address our Hedis prefix.
sharedFleetOverrideKey :: Text -> Text
sharedFleetOverrideKey sourceFleetId = "shared:fleetOverride:sourceFleetId:" <> sourceFleetId

getDispatcherDepotNames ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow [Kernel.Prelude.Text]
  )
getDispatcherDepotNames (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
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
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
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
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
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
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
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
  integratedBPPConfig <- SIBC.findBusIntegratedBPPConfig person.merchantOperatingCityId BecknSpec.BUS DIBC.MULTIMODAL
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
  historyRecords <- B.runInReplica $ QVAH.findAllByDispatcherIdAndAction (Just limit) (Just offset) personId Domain.Types.VehicleActionHistory.DISPATCHER
  pure $ map convertToHistoryRes historyRecords
  where
    convertToHistoryRes :: Domain.Types.VehicleActionHistory.VehicleActionHistory -> API.Types.UI.Dispatcher.DispatcherHistoryRes
    convertToHistoryRes Domain.Types.VehicleActionHistory.VehicleActionHistory {..} =
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
