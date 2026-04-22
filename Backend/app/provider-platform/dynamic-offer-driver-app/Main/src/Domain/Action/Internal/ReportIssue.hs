{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.ReportIssue where

import qualified Data.Aeson as A
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import Domain.Types.ServiceTierType
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import qualified IssueManagement.Common as ICommon
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.Orchestrator as BEOrch
import qualified Lib.BehaviorTracker.Snapshot as BTSnap
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.BehaviourManagement.IssueBreach (IssueBreachType (..))
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import qualified SharedLogic.BehaviourManagement.IssueBreachMitigation as IBM
import SharedLogic.DriverOnboarding
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.DynamicLogic (getAppDynamicLogic)
import Tools.Error
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

reportIssue :: Id Ride -> ICommon.IssueReportType -> Maybe Text -> Flow APISuccess
reportIssue rideId issueType apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  case issueType of
    ICommon.AC_RELATED_ISSUE -> do
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId ride.merchantOperatingCityId Nothing
      -- Keep old AC restriction logic for backward compat
      incrementDriverAcUsageRestrictionCount cityVehicleServiceTiers ride.driverId
      -- Framework pipeline
      driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
      handleAcRestriction ride driverInfo
    ICommon.DRIVER_TOLL_RELATED_ISSUE -> handleTollRelatedIssue ride
    ICommon.SYNC_BOOKING -> pure ()
    ICommon.EXTRA_FARE_MITIGATION -> handleExtraFareMitigation ride booking.vehicleServiceTier
    ICommon.DRUNK_AND_DRIVE_VIOLATION -> handleDrunkAndDriveViolation ride
  return Success

handleTollRelatedIssue :: Ride -> Flow ()
handleTollRelatedIssue ride = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  let tollRelatedIssueCount = fromMaybe 0 driverInfo.tollRelatedIssueCount + 1
  -- Keep DB counter for backward compat
  void $ QDI.updateTollRelatedIssueCount (Just tollRelatedIssueCount) ride.driverId
  -- Framework pipeline
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId (Just (DriverId (cast ride.driverId))) >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  let counterConfig = BTT.CounterConfig {windowSizeDays = 30, counters = [BTT.ACTION_COUNT], periods = [BTT.mkPeriodConfig "window" 30]}
  eventTime <- getCurrentTime
  let actionEvent =
        BTT.ActionEvent
          { entityType = BTT.DRIVER,
            entityId = ride.driverId.getId,
            actionType = "TOLL_RELATED_ISSUE",
            merchantOperatingCityId = ride.merchantOperatingCityId.getId,
            flowContext = A.object [],
            eventData = A.object ["tollRelatedIssueCount" A..= tollRelatedIssueCount],
            timestamp = eventTime
          }
  void $ runBehaviorPipeline transporterConfig ride.driverId ride.merchantOperatingCityId counterConfig actionEvent [] LYT.TOLL_ISSUE_BEHAVIOR

handleExtraFareMitigation :: Ride -> ServiceTierType -> Flow ()
handleExtraFareMitigation ride serviceTierType = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId (Just (DriverId (cast ride.driverId))) >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  let ibConfig = IBM.getIssueBreachConfig EXTRA_FARE_MITIGATION transporterConfig
  let allowedSTiers = ibConfig <&> (.ibAllowedServiceTiers)
  let isRideAllowedForCounting = maybe False (\allowedServiceTiers -> null allowedServiceTiers || serviceTierType `elem` allowedServiceTiers) allowedSTiers
  when isRideAllowedForCounting $
    whenJust ibConfig $ \config -> do
      QDI.updateExtraFareMitigation (pure True) ride.driverId
      -- Keep old counter increment for backward compat
      IBM.incrementIssueBreachCounter EXTRA_FARE_MITIGATION ride.driverId (toInteger config.ibCountWindowSizeInDays)
      -- Framework pipeline
      let windowSize = toInteger config.ibCountWindowSizeInDays
          counterConfig =
            BTT.CounterConfig
              { windowSizeDays = windowSize,
                counters = [BTT.ACTION_COUNT, BTT.ELIGIBLE_COUNT],
                periods =
                  [ BTT.mkPeriodConfig "daily" 1,
                    BTT.mkPeriodConfig "weekly" 7
                  ]
              }
      eventTime <- getCurrentTime
      let actionEvent =
            BTT.ActionEvent
              { entityType = BTT.DRIVER,
                entityId = ride.driverId.getId,
                actionType = "ISSUE_BREACH_EXTRA_FARE",
                merchantOperatingCityId = ride.merchantOperatingCityId.getId,
                flowContext = A.object [],
                eventData =
                  A.object
                    [ "issueType" A..= ("EXTRA_FARE_MITIGATION" :: Text),
                      "serviceTierType" A..= (show serviceTierType :: Text),
                      "blocked" A..= driverInfo.blocked,
                      "softBlockActive" A..= isJust driverInfo.softBlockStiers,
                      "onRide" A..= driverInfo.onRide
                    ],
                timestamp = eventTime
              }
      void $ runBehaviorPipeline transporterConfig ride.driverId ride.merchantOperatingCityId counterConfig actionEvent ["ExtraFareDaily", "ExtraFareWeekly"] LYT.ISSUE_BREACH_BEHAVIOR

handleDrunkAndDriveViolation :: Ride -> Flow ()
handleDrunkAndDriveViolation ride = do
  driverInfo <- QDI.findById ride.driverId >>= fromMaybeM DriverInfoNotFound
  let drunkAndDriveViolationCount = fromMaybe 0 driverInfo.drunkAndDriveViolationCount + 1
  -- Keep DB counter for backward compat
  void $ QDI.updateDrunkAndDriveViolationCount (Just drunkAndDriveViolationCount) ride.driverId
  -- Framework pipeline
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId (Just (DriverId (cast ride.driverId))) >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  let counterConfig = BTT.CounterConfig {windowSizeDays = 365, counters = [BTT.ACTION_COUNT], periods = []}
  eventTime <- getCurrentTime
  let actionEvent =
        BTT.ActionEvent
          { entityType = BTT.DRIVER,
            entityId = ride.driverId.getId,
            actionType = "DRUNK_AND_DRIVE",
            merchantOperatingCityId = ride.merchantOperatingCityId.getId,
            flowContext = A.object [],
            eventData = A.object ["violationCount" A..= drunkAndDriveViolationCount],
            timestamp = eventTime
          }
  handled <- runBehaviorPipeline transporterConfig ride.driverId ride.merchantOperatingCityId counterConfig actionEvent [] LYT.DRUNK_DRIVE_BEHAVIOR
  -- Fallback: if no rules configured, use old hardcoded logic
  unless handled $ do
    logInfo $ "No DRUNK_DRIVE_BEHAVIOR rules configured, using fallback for driver " <> ride.driverId.getId
    person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    if drunkAndDriveViolationCount < 2
      then do
        mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory person.merchantOperatingCityId "DRUNK_AND_DRIVE_WARNING" (fromMaybe ENGLISH person.language) Nothing Nothing Nothing
        whenJust mbOverlay $ \overlay -> do
          let fcmOverlayReq = Notify.mkOverlayReq overlay
          let entityData = Notify.DrunkAndDriveViolationWarningData {driverId = ride.driverId.getId, drunkAndDriveViolationCount}
          Notify.drunkAndDriveViolationWarningOverlay person.merchantOperatingCityId person fcmOverlayReq entityData
      else QDriverInfo.updateDynamicBlockedStateWithActivity ride.driverId (Just "DRUNK_AND_DRIVE_VIOLATION") Nothing "AUTOMATICALLY_BLOCKED_BY_APP" person.merchantId "AUTOMATICALLY_BLOCKED_BY_APP" ride.merchantOperatingCityId DTDBT.Application True Nothing Nothing DrunkAndDriveViolation

handleAcRestriction :: Ride -> DI.DriverInformation -> Flow ()
handleAcRestriction ride driverInfo = do
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId (Just (DriverId (cast ride.driverId))) >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  let airConditionScore = fromMaybe 0 driverInfo.airConditionScore
      counterConfig = BTT.CounterConfig {windowSizeDays = 365, counters = [BTT.ACTION_COUNT], periods = []}
  eventTime <- getCurrentTime
  let actionEvent =
        BTT.ActionEvent
          { entityType = BTT.DRIVER,
            entityId = ride.driverId.getId,
            actionType = "AC_RESTRICTION",
            merchantOperatingCityId = ride.merchantOperatingCityId.getId,
            flowContext = A.object [],
            eventData =
              A.object
                [ "airConditionScore" A..= (airConditionScore + 1),
                  "acUsageRestrictionType" A..= (show driverInfo.acUsageRestrictionType :: Text)
                ],
            timestamp = eventTime
          }
  void $ runBehaviorPipeline transporterConfig ride.driverId ride.merchantOperatingCityId counterConfig actionEvent [] LYT.AC_RESTRICTION_BEHAVIOR

-- | Shared helper to run behavior pipeline for any domain.
-- Returns True if consequences were dispatched, False if no rules/consequences found.
runBehaviorPipeline ::
  DTC.TransporterConfig ->
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  BTT.CounterConfig ->
  BTT.ActionEvent ->
  [Text] -> -- cooldownTags
  LYT.LogicDomain ->
  Flow Bool
runBehaviorPipeline transporterConfig driverId merchantOpCityId counterConfig actionEvent cooldownTags domain = do
  snapshot <- BTSnap.buildSnapshotWithCooldowns counterConfig actionEvent (A.object []) cooldownTags
  let fetchRules = \dom -> do
        localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        getAppDynamicLogic (cast merchantOpCityId) dom localTime Nothing Nothing
  output <- BEOrch.orchestrate snapshot LYDL.Driver (cast merchantOpCityId) domain fetchRules
  let hasConsequences = not (null output.consequences)
      hasCommunications = not (null output.communications)
  logInfo $ "BehaviorPipeline [" <> show domain <> "] for driver " <> driverId.getId <> ": consequences=" <> show (length output.consequences) <> ", communications=" <> show (length output.communications)
  when (hasConsequences || hasCommunications) $ do
    let dispatchCtx =
          BehaviorDispatch.DispatchContext
            { merchantId = transporterConfig.merchantId,
              merchantOperatingCityId = merchantOpCityId,
              counterConfig = Just counterConfig,
              actionEvent = Just actionEvent
            }
    BehaviorDispatch.handleConsequences dispatchCtx driverId output.consequences
    BehaviorDispatch.handleCommunications driverId output.communications
  return (hasConsequences || hasCommunications)
