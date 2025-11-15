module SharedLogic.WMB where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
import API.Types.UI.WMB
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.Alert
import Domain.Types.AlertRequest
import Domain.Types.Common
import Domain.Types.EmptyDynamicParam
import Domain.Types.FleetBadge
import Domain.Types.FleetBadgeAssociation
import qualified Domain.Types.FleetBadgeType as DFBT
import Domain.Types.FleetConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import Domain.Types.Route
import Domain.Types.TripAlertRequest
import Domain.Types.TripTransaction
import qualified Domain.Types.TripTransaction as DTT
import Domain.Types.VehicleRegistrationCertificate
import Domain.Types.VehicleRouteMapping
import qualified Domain.Types.VehicleVariant as DVehVariant
import Domain.Utils
import Environment
import qualified EulerHS.Prelude as EHS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.AlertRequest as QAR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetBadge as QFB
import qualified Storage.Queries.FleetBadgeAssociation as QFBA
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.RouteTripStopMapping as QRTS
import qualified Storage.Queries.TripAlertRequest as QTAR
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRouteMapping as VRM
import Tools.Error
import qualified Tools.Notifications as TN

data StopData = StopData
  { tripCode :: Text,
    routeCode :: Text,
    stopCode :: Text,
    stopLocation :: LatLong
  }

findClosestStop :: Text -> LatLong -> Flow (Maybe StopData)
findClosestStop routeCode loc = do
  now <- getCurrentTime
  allRTSList <- QRTS.findAllRTSMappingByRouteAndDay routeCode (utctTimeToDayOfWeek now)
  if null allRTSList
    then return Nothing
    else do
      let closestStop = minimumBy (EHS.comparing (KU.distanceBetweenInMeters loc . (.stopPoint))) allRTSList
      return $ Just (StopData closestStop.tripCode closestStop.routeCode closestStop.stopCode closestStop.stopPoint)

checkOnboardingCategoryForFleetDriverAssociation :: Id Person -> Id Person -> Flow Bool
checkOnboardingCategoryForFleetDriverAssociation fleetOwnerId driverId = do
  mbFleetDriverAssociation <- QFDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId.getId True
  case mbFleetDriverAssociation of
    Nothing -> return False
    Just fleetDriverAssociation -> return (isJust fleetDriverAssociation.onboardingVehicleCategory)

getSourceAndDestinationStopInfo :: Route -> Text -> Flow (StopInfo, StopInfo)
getSourceAndDestinationStopInfo route routeCode = do
  now <- getCurrentTime
  stops <- QRTS.findAllByRouteCodeForStops routeCode 1 (utctTimeToDayOfWeek now)
  nonEmptySortedStops <-
    case stops of
      [] -> throwError $ RouteTripStopMappingNotFound routeCode
      (a : as) -> return $ NE.sortBy (EHS.comparing (.stopSequenceNum)) (a :| as)
  let sourceRouteTripMapping = NE.head nonEmptySortedStops
      destinationRouteTripMapping = NE.last nonEmptySortedStops
  pure (createStopInfo sourceRouteTripMapping route.startPoint, createStopInfo destinationRouteTripMapping route.endPoint)
  where
    createStopInfo routeTripMapping point =
      StopInfo
        { name = routeTripMapping.stopName,
          code = routeTripMapping.stopCode,
          ..
        }

findNextEligibleTripTransactionByDriverIdStatus :: Text -> Id Person -> TripStatus -> Flow (Maybe TripTransaction)
findNextEligibleTripTransactionByDriverIdStatus fleetOwnerId driverId status = do
  let sortType = if isNonTerminalTripStatus status then QTT.SortAsc else QTT.SortDesc
  QTT.findAllTripTransactionByDriverIdStatus (Id fleetOwnerId) driverId (Just 1) (Just 0) (Just status) sortType >>= \case
    (trip : _) -> pure (Just trip)
    [] -> pure Nothing

findNextEligibleTripTransactionByDriverIdStatusForPilot :: Text -> Id Person -> TripStatus -> Flow (Maybe TripTransaction)
findNextEligibleTripTransactionByDriverIdStatusForPilot fleetOwnerId driverId status = do
  let sortType = if isNonTerminalTripStatus status then QTT.SortAsc else QTT.SortDesc
  QTT.findAllTripTransactionByDriverIdStatusForPilot (Id fleetOwnerId) driverId (Just 1) (Just 0) (Just status) sortType >>= \case
    (trip : _) -> pure (Just trip)
    [] -> pure Nothing

isNonTerminalTripStatus :: TripStatus -> Bool
isNonTerminalTripStatus status = any (== status) [TRIP_ASSIGNED, IN_PROGRESS, UPCOMING]

buildTripAssignedData :: Id TripTransaction -> ServiceTierType -> Text -> Text -> Maybe Text -> (Maybe Text) -> Bool -> TN.WMBTripAssignedData
buildTripAssignedData tripTransactionId vehicleServiceTier vehicleNumber routeCode shortName roundRouteCode isFirstBatchTrip =
  TN.WMBTripAssignedData
    { tripTransactionId = tripTransactionId,
      routeCode = routeCode,
      routeShortname = fromMaybe "None" shortName,
      vehicleNumber = vehicleNumber,
      vehicleServiceTierType = vehicleServiceTier,
      roundRouteCode = roundRouteCode,
      isFirstBatchTrip = isFirstBatchTrip,
      status = TRIP_ASSIGNED
    }

assignAndStartTripTransaction :: FleetConfig -> Id Merchant -> Id MerchantOperatingCity -> Id Person -> Route -> VehicleRouteMapping -> Text -> StopInfo -> StopInfo -> LatLong -> ActionSource -> Maybe (Id FleetBadge) -> Maybe Text -> Maybe (Id FleetBadge) -> Maybe Text -> Flow TripTransaction
assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber startStopInfo destinationStopInfo currentLocation tripStartSource driverFleetBadgeId driverName conductorFleetBadgeId conductorName = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey driverId TRIP_ASSIGNED <> tripTransactionKey driverId IN_PROGRESS)
    60
    ( do
        closestStop <- findClosestStop route.code currentLocation >>= fromMaybeM (StopNotFound)
        tripTransaction <- assignTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber currentLocation startStopInfo destinationStopInfo driverFleetBadgeId driverName conductorFleetBadgeId conductorName
        startTripTransaction tripTransaction (Just route) (Just closestStop) startStopInfo currentLocation destinationStopInfo.point True tripStartSource
    )
    >>= \case
      Right tripTransaction -> return tripTransaction
      Left _ -> throwError (InternalError "Process for Trip Assignment & Start is Already Ongoing, Please try again!")

assignTripTransaction :: FleetConfig -> Id Merchant -> Id MerchantOperatingCity -> Id Person -> Route -> VehicleRouteMapping -> Text -> LatLong -> StopInfo -> StopInfo -> Maybe (Id FleetBadge) -> Maybe Text -> Maybe (Id FleetBadge) -> Maybe Text -> Flow TripTransaction
assignTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber currentLocation startStopInfo destinationStopInfo driverFleetBadgeId driverName conductorFleetBadgeId conductorName = do
  vrc <- validateVehicleAssignment driverId vehicleNumber merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId
  _ <- linkVehicleToDriver driverId merchantId merchantOperatingCityId fleetConfig fleetConfig.fleetOwnerId.getId vehicleNumber vrc
  tripTransaction <- buildTripAssignedTransaction vrc
  -- TODO :: Handle Transaction Failure
  QTT.create tripTransaction
  postAssignTripTransaction tripTransaction (Just route) False currentLocation startStopInfo.point destinationStopInfo.point False
  return tripTransaction
  where
    buildTripAssignedTransaction vrc = do
      tripTransactionId <- generateGUID
      now <- getCurrentTime
      return $
        TripTransaction
          { allowEndingMidRoute = fleetConfig.allowEndingMidRoute,
            deviationCount = 0,
            driverId = driverId,
            endLocation = Nothing,
            fleetOwnerId = vehicleRouteMapping.fleetOwnerId,
            id = tripTransactionId,
            isCurrentlyDeviated = False,
            routeCode = route.code,
            roundRouteCode = route.roundRouteCode,
            status = TRIP_ASSIGNED,
            startLocation = Nothing,
            startedNearStopCode = Nothing,
            tripCode = Nothing,
            vehicleServiceTierType = maybe BUS_NON_AC DVehVariant.castVariantToServiceTier vrc.vehicleVariant,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            endRideApprovalRequestId = Nothing,
            tripStartTime = Just now,
            tripEndTime = Nothing,
            tripTerminationSource = Nothing,
            tripStartSource = Nothing,
            endStopCode = destinationStopInfo.code,
            driverName = driverName,
            conductorName = conductorName,
            driverFleetBadgeId = driverFleetBadgeId,
            conductorFleetBadgeId = conductorFleetBadgeId,
            dutyType = Nothing,
            vipName = Nothing,
            endAddress = Nothing,
            startAddress = Nothing,
            scheduledTripTime = Nothing,
            tripType = Nothing,
            pilotSource = Nothing,
            pilotDestination = Nothing,
            ..
          }

endOngoingTripTransaction :: FleetConfig -> TripTransaction -> LatLong -> ActionSource -> Bool -> Flow ()
endOngoingTripTransaction fleetConfig tripTransaction currentLocation tripTerminationSource isCancelled = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId COMPLETED)
    60
    ( do
        unless (tripTransaction.status == IN_PROGRESS) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        now <- getCurrentTime

        -- Build trip info based on trip type
        tripInfo <- case tripTransaction.tripType of
          Just PILOT -> do
            let source = fromMaybe currentLocation tripTransaction.startLocation
            buildPilotTripInfo tripTransaction source currentLocation
          _ -> do
            let source = fromMaybe currentLocation tripTransaction.startLocation
            pure $
              LT.Bus $
                LT.BusRideInfo
                  tripTransaction.routeCode
                  tripTransaction.vehicleNumber
                  source
                  currentLocation
                  Nothing
                  Nothing
                  (Just fleetConfig.fleetOwnerId.getId)

        void $ LF.rideEnd (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId Nothing (Just tripInfo)
        QTT.updateOnEnd (if isCancelled then CANCELLED else COMPLETED) (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
        TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId COMPLETED "Ride Ended" "Your ride has ended" EmptyDynamicParam

        -- Handle next trip assignment - skip for PILOT trips
        case tripTransaction.tripType of
          Just PILOT -> do
            -- For PILOT, just update driver status
            QDI.updateOnRide False tripTransaction.driverId
            findNextEligibleTripTransactionByDriverIdStatusForPilot tripTransaction.fleetOwnerId.getId tripTransaction.driverId TRIP_ASSIGNED >>= \case
              Just advancedTripTransaction -> do
                psource <- maybe (throwError (InternalError "Pilot source not found")) pure tripTransaction.pilotSource
                pdestination <- maybe (throwError (InternalError "Pilot destination not found")) pure tripTransaction.pilotDestination
                postAssignTripTransaction advancedTripTransaction Nothing False currentLocation psource pdestination True
              Nothing -> do
                findNextEligibleTripTransactionByDriverIdStatusForPilot tripTransaction.fleetOwnerId.getId tripTransaction.driverId UPCOMING >>= \case
                  Just advancedTripTransaction -> void $ assignUpcomingTripTransaction advancedTripTransaction currentLocation
                  Nothing -> pure ()
          _ -> do
            -- Existing WIMB logic for finding next trip
            findNextEligibleTripTransactionByDriverIdStatus tripTransaction.fleetOwnerId.getId tripTransaction.driverId TRIP_ASSIGNED >>= \case
              Just advancedTripTransaction -> do
                route <- QR.findByRouteCode advancedTripTransaction.routeCode >>= fromMaybeM (RouteNotFound advancedTripTransaction.routeCode)
                (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route advancedTripTransaction.routeCode
                postAssignTripTransaction advancedTripTransaction (Just route) False currentLocation sourceStopInfo.point destinationStopInfo.point True
              Nothing -> do
                findNextEligibleTripTransactionByDriverIdStatus tripTransaction.fleetOwnerId.getId tripTransaction.driverId UPCOMING >>= \case
                  Just advancedTripTransaction -> void $ assignUpcomingTripTransaction advancedTripTransaction currentLocation
                  Nothing -> do
                    if fleetConfig.allowAutomaticRoundTripAssignment && isJust tripTransaction.roundRouteCode
                      then do
                        whenJust tripTransaction.roundRouteCode $ \roundRouteCode -> do
                          route <- QR.findByRouteCode roundRouteCode >>= fromMaybeM (RouteNotFound roundRouteCode)
                          (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
                          vehicleNumberHash <- getDbHash tripTransaction.vehicleNumber
                          vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash roundRouteCode >>= fromMaybeM (VehicleRouteMappingNotFound tripTransaction.vehicleNumber roundRouteCode)
                          when (not vehicleRouteMapping.blocked) $ do
                            void $ assignTripTransaction fleetConfig tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.driverId route vehicleRouteMapping tripTransaction.vehicleNumber currentLocation sourceStopInfo destinationStopInfo tripTransaction.driverFleetBadgeId tripTransaction.driverName tripTransaction.conductorFleetBadgeId tripTransaction.conductorName
                      else do
                        QDI.updateOnRide False tripTransaction.driverId
                        when fleetConfig.unlinkDriverAndVehicleOnTripTermination $ unlinkVehicleToDriver tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber
                        unlinkFleetBadgeFromDriver tripTransaction.driverId
    )
    >>= \case
      Right _ -> return ()
      Left _ -> throwError (InternalError "Process for Trip End is Already Ongoing, Please try again!")

cancelTripTransaction :: FleetConfig -> TripTransaction -> LatLong -> ActionSource -> Flow ()
cancelTripTransaction fleetConfig tripTransaction currentLocation tripTerminationSource = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId CANCELLED)
    60
    ( do
        unless (tripTransaction.status `elem` [IN_PROGRESS, TRIP_ASSIGNED, UPCOMING]) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        findNextActiveTripTransaction tripTransaction.fleetOwnerId.getId tripTransaction.driverId
          >>= \case
            Nothing -> pure ()
            Just currentTripTransaction -> do
              let isCurrentlyOngoingTrip = currentTripTransaction.id == tripTransaction.id
              if isCurrentlyOngoingTrip
                then do
                  case currentTripTransaction.status of
                    TRIP_ASSIGNED -> do
                      now <- getCurrentTime
                      void $ LF.rideDetails (cast tripTransaction.id) DRide.CANCELLED tripTransaction.merchantId tripTransaction.driverId currentLocation.lat currentLocation.lon Nothing Nothing
                      QTT.updateOnEnd CANCELLED (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
                      TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId CANCELLED "Ride Cancelled" "Your ride has been Cancelled" EmptyDynamicParam
                      runInMasterDbAndRedis $
                        -- Skip route-based operations for PILOT trips
                        case tripTransaction.tripType of
                          Just PILOT -> do
                            QDI.updateOnRide False tripTransaction.driverId
                            findNextEligibleTripTransactionByDriverIdStatusForPilot tripTransaction.fleetOwnerId.getId tripTransaction.driverId TRIP_ASSIGNED >>= \case
                              Just advancedTripTransaction -> do
                                psource <- maybe (throwError (InternalError "Pilot source not found")) pure tripTransaction.pilotSource
                                pdestination <- maybe (throwError (InternalError "Pilot destination not found")) pure tripTransaction.pilotDestination
                                postAssignTripTransaction advancedTripTransaction Nothing False currentLocation psource pdestination True
                              Nothing -> do
                                findNextEligibleTripTransactionByDriverIdStatusForPilot tripTransaction.fleetOwnerId.getId tripTransaction.driverId UPCOMING >>= \case
                                  Just advancedTripTransaction -> void $ assignUpcomingTripTransaction advancedTripTransaction currentLocation
                                  Nothing -> pure ()
                          _ -> do
                            findNextEligibleTripTransactionByDriverIdStatus tripTransaction.fleetOwnerId.getId tripTransaction.driverId TRIP_ASSIGNED >>= \case
                              Just advancedTripTransaction -> do
                                route <- QR.findByRouteCode advancedTripTransaction.routeCode >>= fromMaybeM (RouteNotFound advancedTripTransaction.routeCode)
                                (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route advancedTripTransaction.routeCode
                                postAssignTripTransaction advancedTripTransaction (Just route) False currentLocation sourceStopInfo.point destinationStopInfo.point True
                              Nothing -> do
                                findNextEligibleTripTransactionByDriverIdStatus tripTransaction.fleetOwnerId.getId tripTransaction.driverId UPCOMING >>= \case
                                  Just advancedTripTransaction -> void $ assignUpcomingTripTransaction advancedTripTransaction currentLocation
                                  Nothing -> do
                                    QDI.updateOnRide False tripTransaction.driverId
                                    when fleetConfig.unlinkDriverAndVehicleOnTripTermination $ unlinkVehicleToDriver tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber
                                    unlinkFleetBadgeFromDriver tripTransaction.driverId
                    IN_PROGRESS -> endOngoingTripTransaction fleetConfig tripTransaction currentLocation tripTerminationSource True
                    _ -> pure ()
                else do
                  now <- getCurrentTime
                  QTT.updateOnEnd CANCELLED (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
    )
    >>= \case
      Right _ -> return ()
      Left _ -> throwError (InternalError "Process for Trip Cancellation is Already Ongoing, Please try again!")

buildBusTripInfo :: Text -> Text -> LatLong -> LatLong -> Text -> Id Person -> Text -> Flow LT.RideInfo
buildBusTripInfo vehicleNumber routeCode sourceLocation destinationLocation longName driverId fleetOwnerId = do
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  return $
    LT.Bus $
      LT.BusRideInfo
        { busNumber = vehicleNumber,
          source = sourceLocation,
          destination = destinationLocation,
          routeLongName = (Just longName),
          driverName = Just $ driver.firstName <> maybe "" (" " <>) driver.lastName,
          groupId = Just fleetOwnerId,
          ..
        }

buildPilotTripInfo :: TripTransaction -> LatLong -> LatLong -> Flow LT.RideInfo
buildPilotTripInfo tripTransaction source destination = do
  return $
    LT.Pilot $
      LT.PilotRideInfo
        { pilotNumber = tripTransaction.vehicleNumber,
          source = source,
          destination = destination,
          driverName = tripTransaction.driverName,
          dutyType = tripTransaction.dutyType,
          endAddress = tripTransaction.endAddress,
          groupId = Just tripTransaction.fleetOwnerId.getId,
          scheduledTripTime = tripTransaction.scheduledTripTime,
          startAddress = tripTransaction.startAddress,
          vipName = tripTransaction.vipName
        }

postAssignTripTransaction :: TripTransaction -> Maybe Route -> Bool -> LatLong -> LatLong -> LatLong -> Bool -> Flow ()
postAssignTripTransaction tripTransaction mbRoute isFirstBatchTrip currentLocation source destination notify = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId TRIP_ASSIGNED)
    60
    ( do
        unless (tripTransaction.status == TRIP_ASSIGNED || tripTransaction.status == UPCOMING) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        tripInfo <- case tripTransaction.tripType of
          Just PILOT -> do
            buildPilotTripInfo tripTransaction source destination
          _ -> do
            let longName = fromMaybe "None" (mbRoute <&> (.longName))
            buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode source destination longName tripTransaction.driverId tripTransaction.fleetOwnerId.getId
        void $ LF.rideDetails (cast tripTransaction.id) DRide.NEW tripTransaction.merchantId tripTransaction.driverId currentLocation.lat currentLocation.lon Nothing (Just tripInfo)
        QDI.updateOnRide True tripTransaction.driverId
        when notify $ do
          let tripAssignedEntityData = buildTripAssignedData tripTransaction.id tripTransaction.vehicleServiceTierType tripTransaction.vehicleNumber tripTransaction.routeCode (mbRoute <&> (.shortName)) (mbRoute >>= (.roundRouteCode)) isFirstBatchTrip
          TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId TRIP_ASSIGNED "Ride Assigned" "Ride assigned" tripAssignedEntityData
    )
    >>= \case
      Right _ -> return ()
      Left _ -> throwError (InternalError "Process for Trip Assignment is Already Ongoing, Please try again!")

startTripTransaction :: TripTransaction -> Maybe Route -> Maybe StopData -> StopInfo -> LatLong -> LatLong -> Bool -> ActionSource -> Flow TripTransaction
startTripTransaction tripTransaction mbRoute mbClosestStop sourceStopInfo currentLocation destination notify tripStartSource = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId IN_PROGRESS)
    60
    ( do
        unless (tripTransaction.status == TRIP_ASSIGNED) $ throwError (InvalidTripStatus $ show tripTransaction.status)

        -- Build trip info based on trip type
        tripInfo <- case tripTransaction.tripType of
          Just PILOT -> do
            let source = fromMaybe currentLocation tripTransaction.pilotSource
            let dest = fromMaybe destination tripTransaction.pilotDestination
            buildPilotTripInfo tripTransaction source dest
          _ -> do
            route <- fromMaybeM (RouteNotFound tripTransaction.routeCode) mbRoute
            buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode sourceStopInfo.point destination route.longName tripTransaction.driverId tripTransaction.fleetOwnerId.getId

        void $ LF.rideStart (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId (Just tripInfo)
        now <- getCurrentTime
        QDI.updateOnRide True tripTransaction.driverId

        -- Handle tripCode and startedNearStopCode based on trip type
        let (tripCode', startedNearStopCode') = case tripTransaction.tripType of
              Just PILOT -> (tripTransaction.tripCode, tripTransaction.startedNearStopCode) -- Keep existing or use vehicleNumber
              _ -> (mbClosestStop <&> (.tripCode), mbClosestStop <&> (.stopCode))

        let tripStartTransaction = tripTransaction {tripCode = tripCode', startedNearStopCode = startedNearStopCode', startLocation = Just currentLocation, status = IN_PROGRESS, tripStartTime = Just now}
        QTT.updateOnStart tripStartTransaction.tripCode tripStartTransaction.startedNearStopCode tripStartTransaction.startLocation tripStartTransaction.status tripStartTransaction.tripStartTime (Just tripStartSource) tripStartTransaction.id
        when notify $ do
          TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId IN_PROGRESS "Ride Started" "Your ride has started" EmptyDynamicParam
        -- Only check wrong start stop for WIMB trips
        when (tripTransaction.tripType /= Just PILOT) $ do
          fork "Check Wrong Start Stop" $ checkWrongStartStop tripStartTransaction
        return tripStartTransaction
    )
    >>= \case
      Right tripStartTransaction -> return tripStartTransaction
      Left _ -> throwError (InternalError "Process for Trip Start is Already Ongoing, Please try again!")
  where
    checkWrongStartStop :: TripTransaction -> Flow ()
    checkWrongStartStop tripStartTransaction = do
      -- Only check for WIMB trips with routes
      whenJust mbRoute $ \_route -> do
        unless (Just sourceStopInfo.code == tripStartTransaction.startedNearStopCode) $ do
          driver <- QP.findById tripStartTransaction.driverId >>= fromMaybeM (PersonNotFound tripStartTransaction.driverId.getId)
          mobileNumber <- mapM decrypt driver.mobileNumber
          let requestData =
                WrongStartStop
                  WrongStartStopData
                    { driverMobileNumber = mobileNumber,
                      driverName = fromMaybe driver.firstName tripStartTransaction.driverName,
                      location = currentLocation,
                      stopName = sourceStopInfo.name,
                      distance = Just $ distanceBetweenInMeters currentLocation sourceStopInfo.point
                    }
          void $ triggerAlertRequest tripStartTransaction.driverId tripStartTransaction.fleetOwnerId.getId "Trip started from wrong start stop!" "Your trip has started from wrong start stop!" requestData True tripTransaction

validateVehicleAssignment :: Id Person -> Text -> Id Merchant -> Id MerchantOperatingCity -> Text -> Flow (VehicleRegistrationCertificate)
validateVehicleAssignment driverId vehicleNumber _ _ fleetOwnerId = do
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNumber >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId)
  unless (vehicleRC.verificationStatus == Documents.VALID) $ throwError (RcNotValid)
  findNextActiveTripTransaction fleetOwnerId driverId
    >>= \case
      Nothing -> pure ()
      Just tripTransaction ->
        if tripTransaction.vehicleNumber /= vehicleNumber
          then throwError $ AlreadyOnActiveTripWithAnotherVehicle tripTransaction.vehicleNumber
          else pure ()
  QV.findByRegistrationNo vehicleNumber >>= \case
    Just vehicle -> do
      when (vehicle.driverId /= driverId) $ throwError (VehicleLinkedToAnotherDriver vehicleNumber)
      pure ()
    Nothing -> pure ()
  return vehicleRC

validateBadgeAssignment :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Text -> DFBT.FleetBadgeType -> Flow (FleetBadge)
validateBadgeAssignment driverId merchantId merchantOperatingCityId fleetOwnerId badgeName badgeType = do
  unless (badgeType `elem` [DFBT.OFFICER, DFBT.PILOT]) $
    findNextActiveTripTransaction fleetOwnerId driverId
      >>= \case
        Nothing -> pure ()
        Just tripTransaction ->
          whenJust tripTransaction.driverName $ \driverName ->
            when (driverName /= badgeName) $ throwError (AlreadyOnActiveTripWithAnotherBadge driverName)
  badge <-
    QFB.findOneBadgeByNameAndBadgeTypeAndFleetOwnerId (Id fleetOwnerId) badgeName badgeType
      >>= \case
        Just a -> return a
        Nothing -> createNewBadge
  driverBadge <- QFBA.findActiveFleetBadgeAssociationById badge.id badgeType
  case driverBadge of
    Just dBadge -> when (dBadge.driverId.getId /= driverId.getId) $ throwError (FleetBadgeAlreadyLinked dBadge.driverId.getId)
    Nothing -> pure ()
  return badge
  where
    createNewBadge = do
      now <- getCurrentTime
      badgeId <- generateGUID
      let newBadge = buildNewBadge badgeId now
      QFB.create newBadge
      pure newBadge

    buildNewBadge badgeId now =
      FleetBadge
        { badgeName = badgeName,
          badgeType = badgeType,
          createdAt = now,
          personId = Nothing,
          fleetOwnerId = Id fleetOwnerId,
          id = Id badgeId,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          updatedAt = now,
          badgeRank = Nothing
        }

-- TODO :: Unlink Fleet Badge Driver to be Figured Out, If Required
linkFleetBadge :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> FleetBadge -> DFBT.FleetBadgeType -> Flow ()
linkFleetBadge driverId _ _ fleetOwnerId badge badgeType = do
  createBadgeAssociation -- Upsert the badge association
  let (driverBadgeName, conductorBadgeName) = case badgeType of
        DFBT.DRIVER -> (Just badge.badgeName, Nothing)
        DFBT.CONDUCTOR -> (Nothing, Just badge.badgeName)
        DFBT.PILOT -> (Just badge.badgeName, badge.badgeRank)
        DFBT.OFFICER -> (Just badge.badgeName, badge.badgeRank)
  QP.updatePersonName driverId driverBadgeName conductorBadgeName
  where
    createBadgeAssociation = do
      now <- getCurrentTime
      fleetBadgeId <- generateGUID
      let fleetBadgeAssoc = buildBadgeAssociation fleetBadgeId now
      QFBA.createBadgeAssociationIfNotExists fleetBadgeAssoc

    buildBadgeAssociation fleetBadgeId now =
      FleetBadgeAssociation
        { associatedOn = Just now,
          associatedTill = convertTextToUTC (Just "2099-12-12"),
          badgeId = badge.id,
          badgeType = badgeType,
          createdAt = now,
          driverId = driverId,
          fleetOwnerId = fleetOwnerId,
          id = Id fleetBadgeId,
          isActive = True,
          updatedAt = now
        }

linkVehicleToDriver :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> FleetConfig -> Text -> Text -> VehicleRegistrationCertificate -> Flow ()
linkVehicleToDriver driverId merchantId merchantOperatingCityId _ _ vehicleNumber vehicleRC = do
  tryLinkinRC
  where
    tryLinkinRC = do
      now <- getCurrentTime
      mRCAssociation <- DAQuery.findLatestByRCIdAndDriverId vehicleRC.id driverId
      case mRCAssociation of
        Just assoc -> do
          when (maybe True (now >) assoc.associatedTill) $ -- if that association is old, create new association for that driver
            createRCAssociation
        Nothing -> createRCAssociation
      let rcStatusReq =
            DomainRC.RCStatusReq
              { rcNo = vehicleNumber,
                isActivate = True
              }
      void $ DomainRC.linkRCStatus (driverId, merchantId, merchantOperatingCityId) rcStatusReq
    createRCAssociation = do
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
      createDriverRCAssociationIfPossible transporterConfig driverId vehicleRC

-- forceCancelAllActiveTripTransaction vehicleDriverId = do
--   tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus (Just 10) vehicleDriverId
--   if null tripTransactions
--     then pure ()
--     else do
--       currentLocation <- getDriverCurrentLocation vehicleDriverId
--       _ <- mapM (\tripTransaction -> cancelTripTransaction fleetConfig tripTransaction currentLocation ForceDashboard) tripTransactions
--       logDebug $ "Force Cancelled " <> show (length tripTransactions) <> " active trips for driver " <> show vehicleDriverId
--       forceCancelAllActiveTripTransaction vehicleDriverId

unlinkFleetBadgeFromDriver :: Id Person -> Flow ()
unlinkFleetBadgeFromDriver driverId = do
  QFBA.endAssociationForDriver driverId

unlinkVehicleToDriver :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Flow ()
unlinkVehicleToDriver driverId merchantId merchantOperatingCityId vehicleNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = vehicleNumber,
            isActivate = False
          }
  void $ DomainRC.linkRCStatus (driverId, merchantId, merchantOperatingCityId) rcStatusReq
  RCQuery.findLastVehicleRCWrapper vehicleNumber
    >>= \case
      Just rc -> DAQuery.endAssociationForRC driverId rc.id
      Nothing -> pure ()

getRouteDetails :: Text -> Flow Common.RouteDetails
getRouteDetails routeCode = do
  route <- QR.findByRouteCode routeCode >>= fromMaybeM (RouteNotFound routeCode)
  let waypoints = case route.polyline of
        Just polyline -> Just (KEPP.decode polyline)
        Nothing -> Nothing
  now <- getCurrentTime
  stops <- QRTS.findAllByRouteCodeForStops routeCode 1 (utctTimeToDayOfWeek now)
  let stopInfos = map (\stop -> StopInfo (stop.stopCode) (stop.stopName) (stop.stopPoint)) (sortBy (EHS.comparing (.stopSequenceNum)) stops)
  pure $
    Common.RouteDetails
      { code = routeCode,
        shortName = route.shortName,
        longName = route.longName,
        startPoint = route.startPoint,
        endPoint = route.endPoint,
        stops = map castStops stopInfos,
        waypoints = waypoints,
        timeBounds = Just route.timeBounds
      }
  where
    castStops stop = Common.StopInfo stop.name stop.code stop.point

findNextActiveTripTransaction :: Text -> Id Person -> Flow (Maybe TripTransaction)
findNextActiveTripTransaction fleetOwnerId driverId = do
  findNextEligibleTripTransactionByDriverIdStatus fleetOwnerId driverId IN_PROGRESS
    |<|>| findNextEligibleTripTransactionByDriverIdStatus fleetOwnerId driverId TRIP_ASSIGNED
      >>= \case
        Just tripTransaction -> return (Just tripTransaction)
        Nothing ->
          findNextEligibleTripTransactionByDriverIdStatus fleetOwnerId driverId UPCOMING
            >>= \case
              Just tripTransaction -> do
                logError $ "Upcoming trip is assigned to driver by AutoRecovery" <> show tripTransaction.driverId
                mbCurrentDriverLocation <-
                  withTryCatch "driversLocation:findNextActiveTripTransaction" (LF.driversLocation [tripTransaction.driverId])
                    >>= \case
                      Left _ -> do
                        logError "Driver is not active since 24 hours, please ask driver to go online and then end the trip."
                        return Nothing
                      Right locations -> do
                        let location = listToMaybe locations
                        when (isNothing location) $ logError "Driver is not active since 24 hours, please ask driver to go online and then end the trip."
                        return location
                assignedTripTransaction <- assignUpcomingTripTransaction tripTransaction (maybe (LatLong 0.0 0.0) (\currentDriverLocation -> LatLong currentDriverLocation.lat currentDriverLocation.lon) mbCurrentDriverLocation)
                return (Just assignedTripTransaction)
              Nothing -> pure Nothing

-- Redis Transaction Lock Keys --
tripTransactionKey :: Id Person -> TripStatus -> Text
tripTransactionKey driverId = \case
  TRIP_ASSIGNED -> "WMB:TA:" <> driverId.getId
  IN_PROGRESS -> "WMB:TI:" <> driverId.getId
  COMPLETED -> "WMB:TCO:" <> driverId.getId
  CANCELLED -> "WMB:TCA:" <> driverId.getId
  PAUSED -> "WMB:TP:" <> driverId.getId
  UPCOMING -> "WMB:TU:" <> driverId.getId

triggerAlertRequest :: Id Person -> Text -> Text -> Text -> AlertRequestData -> Bool -> TripTransaction -> Flow (Id AlertRequest)
triggerAlertRequest driverId requesteeId title body requestData isViolated tripTransaction = do
  let alertRequestType = castAlertRequestDataToRequestType requestData
  if isViolated
    then do
      alertRequestId <- generateGUID
      now <- getCurrentTime
      let alertRequest =
            AlertRequest
              { id = alertRequestId,
                requestorId = driverId,
                requestorType = DriverGenerated,
                requesteeId = Id requesteeId,
                requesteeType = FleetOwner,
                requestType = castAlertRequestDataToRequestType requestData,
                reason = Nothing,
                status = AWAITING_APPROVAL,
                createdAt = now,
                updatedAt = now,
                merchantId = tripTransaction.merchantId,
                merchantOperatingCityId = tripTransaction.merchantOperatingCityId,
                ..
              }
      QAR.create alertRequest
      tripAlertRequestId <- generateGUID
      QTAR.create $
        TripAlertRequest
          { id = tripAlertRequestId,
            alertRequestId = alertRequestId,
            tripTransactionId = tripTransaction.id,
            driverId = driverId,
            fleetOwnerId = Id requesteeId,
            driverFleetBadgeId = tripTransaction.driverFleetBadgeId,
            conductorFleetBadgeId = tripTransaction.conductorFleetBadgeId,
            routeCode = tripTransaction.routeCode,
            alertRequestType = alertRequestType,
            isViolated = isViolated,
            createdAt = now,
            updatedAt = now,
            merchantId = tripTransaction.merchantId,
            merchantOperatingCityId = tripTransaction.merchantOperatingCityId,
            alertStatus = Just $ AWAITING_APPROVAL
          }
      TN.notifyFleetWithGRPCProvider tripTransaction.merchantOperatingCityId Notification.TRIGGER_FCM title body driverId requestData
      pure alertRequest.id
    else do
      tripAlertRequest <- QTAR.findLatestTripAlertRequest tripTransaction.merchantOperatingCityId tripTransaction.fleetOwnerId.getId alertRequestType driverId.getId tripTransaction.routeCode >>= fromMaybeM (TripAlertRequestNotFound tripTransaction.id.getId)
      QTAR.updateIsViolated False tripAlertRequest.id
      pure tripAlertRequest.alertRequestId

updateAlertRequestStatus :: AlertRequestStatus -> Kernel.Prelude.Maybe Data.Text.Text -> Id AlertRequest -> Flow ()
updateAlertRequestStatus status reason alertRequestId = do
  QAR.updateStatusWithReason status reason alertRequestId
  QTAR.updateStatusWithReason status alertRequestId

assignUpcomingTripTransaction :: TripTransaction -> LatLong -> Flow TripTransaction
assignUpcomingTripTransaction tripTransaction currentLocation = do
  unless (tripTransaction.status == UPCOMING) $ throwError (InvalidTripStatus $ show tripTransaction.status)
  let tripTransactionT = tripTransaction {DTT.status = TRIP_ASSIGNED}
  QTT.updateStatus tripTransactionT.status tripTransaction.id
  case tripTransaction.tripType of
    Just PILOT -> do
      psource <- maybe (throwError (InternalError "Pilot source not found")) pure tripTransaction.pilotSource
      pdestination <- maybe (throwError (InternalError "Pilot destination not found")) pure tripTransaction.pilotDestination
      postAssignTripTransaction tripTransactionT Nothing False currentLocation psource pdestination True
    _ -> do
      route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
      (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route tripTransaction.routeCode
      postAssignTripTransaction tripTransactionT (Just route) False currentLocation sourceStopInfo.point destinationStopInfo.point True
  return tripTransactionT
