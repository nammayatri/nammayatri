module SharedLogic.WMB where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
import API.Types.UI.WMB
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.Alert
import Domain.Types.AlertRequest
import Domain.Types.Common
import Domain.Types.EmptyDynamicParam
import Domain.Types.FleetBadge
import Domain.Types.FleetBadgeAssociation
import Domain.Types.FleetConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import Domain.Types.Route
import Domain.Types.TripAlertRequest
import Domain.Types.TripTransaction
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
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
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

checkFleetDriverAssociation :: Id Person -> Id Person -> Flow Bool
checkFleetDriverAssociation driverId fleetOwnerId = do
  mbFleetDriverAssociation <- QFDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId.getId True
  case mbFleetDriverAssociation of
    Nothing -> return False
    Just fleetDriverAssociation -> do
      if (isJust fleetDriverAssociation.onboardingVehicleCategory)
        then return True
        else return False

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

findNextEligibleTripTransactionByDriverIdStatus :: Id Person -> TripStatus -> Flow (Maybe TripTransaction)
findNextEligibleTripTransactionByDriverIdStatus driverId status = do
  let sortType = if isNonTerminalTripStatus status then QTT.SortAsc else QTT.SortDesc
  QTT.findAllTripTransactionByDriverIdStatus driverId (Just 1) (Just 0) (Just status) sortType >>= \case
    (trip : _) -> pure (Just trip)
    [] -> pure Nothing

isNonTerminalTripStatus :: TripStatus -> Bool
isNonTerminalTripStatus status = any (\status' -> status' == status) [TRIP_ASSIGNED, IN_PROGRESS]

buildTripAssignedData :: Id TripTransaction -> ServiceTierType -> Text -> Text -> Text -> (Maybe Text) -> Bool -> TN.WMBTripAssignedData
buildTripAssignedData tripTransactionId vehicleServiceTier vehicleNumber routeCode shortName roundRouteCode isFirstBatchTrip =
  TN.WMBTripAssignedData
    { tripTransactionId = tripTransactionId,
      routeCode = routeCode,
      routeShortname = shortName,
      vehicleNumber = vehicleNumber,
      vehicleServiceTierType = vehicleServiceTier,
      roundRouteCode = roundRouteCode,
      isFirstBatchTrip = isFirstBatchTrip
    }

assignAndStartTripTransaction :: FleetConfig -> Id Merchant -> Id MerchantOperatingCity -> Id Person -> Route -> VehicleRouteMapping -> Text -> StopInfo -> StopInfo -> LatLong -> Flow TripTransaction
assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber startStopInfo destinationStopInfo currentLocation = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey driverId TRIP_ASSIGNED <> tripTransactionKey driverId IN_PROGRESS)
    60
    ( do
        tripTransactionId <- generateGUID
        now <- getCurrentTime
        closestStop <- findClosestStop route.code currentLocation >>= fromMaybeM (StopNotFound)
        vrc <- validateVehicleAssignment driverId vehicleNumber merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId
        _ <- linkVehicleToDriver driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId vehicleNumber vrc
        driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
        let tripTransaction = buildTripTransaction tripTransactionId destinationStopInfo.code now closestStop vrc (Just driver.firstName)
        -- TODO :: Handle Transaction Failure
        QTT.create tripTransaction
        assignTripTransaction tripTransaction route False currentLocation destinationStopInfo.point False
        startTripTransaction tripTransaction route closestStop startStopInfo currentLocation destinationStopInfo.point True
    )
    >>= \case
      Right tripTransaction -> return tripTransaction
      Left _ -> throwError (InternalError "Process for Trip Assignment & Start is Already Ongoing, Please try again!")
  where
    buildTripTransaction tripTransactionId endStopCode now closestStop vrc firstName =
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
          startLocation = (Just currentLocation),
          startedNearStopCode = (Just closestStop.stopCode),
          tripCode = (Just closestStop.tripCode),
          vehicleServiceTierType = maybe BUS_NON_AC DVehVariant.castVariantToServiceTier vrc.vehicleVariant,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          createdAt = now,
          updatedAt = now,
          endRideApprovalRequestId = Nothing,
          tripStartTime = Just now,
          tripEndTime = Nothing,
          tripTerminationSource = Nothing,
          driverName = firstName,
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
        void $ LF.rideEnd (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId Nothing
        QTT.updateOnEnd (if isCancelled then CANCELLED else COMPLETED) (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
        TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId COMPLETED "Ride Ended" "Your ride has ended" EmptyDynamicParam
        findNextEligibleTripTransactionByDriverIdStatus tripTransaction.driverId TRIP_ASSIGNED >>= \case
          Just advancedTripTransaction -> do
            route <- QR.findByRouteCode advancedTripTransaction.routeCode >>= fromMaybeM (RouteNotFound advancedTripTransaction.routeCode)
            (_, destinationStopInfo) <- getSourceAndDestinationStopInfo route advancedTripTransaction.routeCode
            assignTripTransaction advancedTripTransaction route False currentLocation destinationStopInfo.point True
          Nothing -> do
            if fleetConfig.allowAutomaticRoundTripAssignment
              then do
                whenJust tripTransaction.roundRouteCode $ \roundRouteCode -> do
                  route <- QR.findByRouteCode roundRouteCode >>= fromMaybeM (RouteNotFound roundRouteCode)
                  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
                  vehicleNumberHash <- getDbHash tripTransaction.vehicleNumber
                  vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash roundRouteCode >>= fromMaybeM (VehicleRouteMappingNotFound tripTransaction.vehicleNumber roundRouteCode)
                  when (not vehicleRouteMapping.blocked) $ do
                    void $ assignAndStartTripTransaction fleetConfig tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.driverId route vehicleRouteMapping tripTransaction.vehicleNumber sourceStopInfo destinationStopInfo currentLocation
              else do
                QDI.updateOnRide False tripTransaction.driverId
                unlinkVehicleToDriver tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber
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
        unless (tripTransaction.status `elem` [IN_PROGRESS, TRIP_ASSIGNED]) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        findNextActiveTripTransaction tripTransaction.driverId
          >>= \case
            Nothing -> pure ()
            Just currentTripTransaction -> do
              if currentTripTransaction.id == tripTransaction.id
                then do
                  case currentTripTransaction.status of
                    TRIP_ASSIGNED -> do
                      now <- getCurrentTime
                      void $ LF.rideDetails (cast tripTransaction.id) DRide.CANCELLED tripTransaction.merchantId tripTransaction.driverId currentLocation.lat currentLocation.lon Nothing Nothing
                      QTT.updateOnEnd CANCELLED (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
                      TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId CANCELLED "Ride Cancelled" "Your ride has been Cancelled" EmptyDynamicParam
                      runInMasterDbAndRedis $
                        findNextEligibleTripTransactionByDriverIdStatus tripTransaction.driverId TRIP_ASSIGNED >>= \case
                          Just advancedTripTransaction -> do
                            route <- QR.findByRouteCode advancedTripTransaction.routeCode >>= fromMaybeM (RouteNotFound advancedTripTransaction.routeCode)
                            (_, destinationStopInfo) <- getSourceAndDestinationStopInfo route advancedTripTransaction.routeCode
                            assignTripTransaction advancedTripTransaction route False currentLocation destinationStopInfo.point True
                          Nothing -> do
                            QDI.updateOnRide False tripTransaction.driverId
                            unlinkVehicleToDriver tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber
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

buildBusTripInfo :: Text -> Text -> LatLong -> Text -> Id Person -> Flow LT.RideInfo
buildBusTripInfo vehicleNumber routeCode destinationLocation longName driverId = do
  driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  return $
    LT.Bus $
      LT.BusRideInfo
        { busNumber = vehicleNumber,
          destination = destinationLocation,
          routeLongName = (Just longName),
          driverName = Just $ driver.firstName <> maybe "" (" " <>) driver.lastName,
          ..
        }

assignTripTransaction :: TripTransaction -> Route -> Bool -> LatLong -> LatLong -> Bool -> Flow ()
assignTripTransaction tripTransaction route isFirstBatchTrip currentLocation destination notify = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId TRIP_ASSIGNED)
    60
    ( do
        unless (tripTransaction.status == TRIP_ASSIGNED) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        busTripInfo <- buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode destination route.longName tripTransaction.driverId
        void $ LF.rideDetails (cast tripTransaction.id) DRide.NEW tripTransaction.merchantId tripTransaction.driverId currentLocation.lat currentLocation.lon Nothing (Just busTripInfo)
        QDI.updateOnRide True tripTransaction.driverId
        when notify $ do
          let tripAssignedEntityData = buildTripAssignedData tripTransaction.id tripTransaction.vehicleServiceTierType tripTransaction.vehicleNumber tripTransaction.routeCode route.shortName route.roundRouteCode isFirstBatchTrip
          TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId TRIP_ASSIGNED "Ride Assigned" "Ride assigned" tripAssignedEntityData
    )
    >>= \case
      Right _ -> return ()
      Left _ -> throwError (InternalError "Process for Trip Assignment is Already Ongoing, Please try again!")

startTripTransaction :: TripTransaction -> Route -> StopData -> StopInfo -> LatLong -> LatLong -> Bool -> Flow TripTransaction
startTripTransaction tripTransaction route closestStop sourceStopInfo currentLocation destination notify = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId IN_PROGRESS)
    60
    ( do
        unless (tripTransaction.status == TRIP_ASSIGNED) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        busTripInfo <- buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode destination route.longName tripTransaction.driverId
        void $ LF.rideStart (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId (Just busTripInfo)
        now <- getCurrentTime
        QDI.updateOnRide True tripTransaction.driverId
        let tripStartTransaction = tripTransaction {tripCode = Just closestStop.tripCode, startedNearStopCode = Just closestStop.stopCode, startLocation = Just currentLocation, status = IN_PROGRESS, tripStartTime = Just now}
        QTT.updateOnStart tripStartTransaction.tripCode tripStartTransaction.startedNearStopCode tripStartTransaction.startLocation tripStartTransaction.status tripStartTransaction.tripStartTime tripStartTransaction.id
        when notify $ do
          TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId IN_PROGRESS "Ride Started" "Your ride has started" EmptyDynamicParam
        fork "Check Wrong Start Stop" $ checkWrongStartStop tripStartTransaction
        return tripStartTransaction
    )
    >>= \case
      Right tripStartTransaction -> return tripStartTransaction
      Left _ -> throwError (InternalError "Process for Trip Start is Already Ongoing, Please try again!")
  where
    checkWrongStartStop :: TripTransaction -> Flow ()
    checkWrongStartStop tripStartTransaction = do
      unless (Just sourceStopInfo.code == tripStartTransaction.startedNearStopCode) $ do
        driver <- QP.findById tripStartTransaction.driverId >>= fromMaybeM (PersonNotFound tripStartTransaction.driverId.getId)
        mobileNumber <- mapM decrypt driver.mobileNumber
        let requestData =
              WrongStartStop
                WrongStartStopData
                  { driverMobileNumber = mobileNumber,
                    driverName = fromMaybe driver.firstName tripStartTransaction.driverName,
                    location = currentLocation,
                    stopName = sourceStopInfo.name
                  }
        void $ triggerAlertRequest tripStartTransaction.driverId tripStartTransaction.fleetOwnerId.getId "Trip started from wrong start stop!" "Your trip has started from wrong start stop!" requestData True tripTransaction

validateVehicleAssignment :: Id Person -> Text -> Id Merchant -> Id MerchantOperatingCity -> Text -> Flow (VehicleRegistrationCertificate)
validateVehicleAssignment driverId vehicleNumber _ _ fleetOwnerId = do
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNumber >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId)
  unless (vehicleRC.verificationStatus == Documents.VALID) $ throwError (RcNotValid)
  findNextActiveTripTransaction driverId
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

validateBadgeAssignment :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Text -> Flow (FleetBadge)
validateBadgeAssignment driverId merchantId merchantOperatingCityId fleetOwnerId badgeName = do
  findNextActiveTripTransaction driverId
    >>= \case
      Nothing -> pure ()
      Just tripTransaction ->
        whenJust tripTransaction.driverName $ \driverName ->
          when (driverName /= badgeName) $ throwError (AlreadyOnActiveTripWithAnotherBadge driverName)
  badge <-
    QFB.findOneBadgeByNameAndFleetOwnerId (Id fleetOwnerId) badgeName
      >>= \case
        Just a -> return a
        Nothing -> createNewBadge
  driverBadge <- QFBA.findActiveFleetBadgeAssociationById badge.id
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
          createdAt = now,
          fleetOwnerId = Id fleetOwnerId,
          id = Id badgeId,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          updatedAt = now
        }

-- TODO :: Unlink Fleet Badge Driver to be Figured Out, If Required
linkFleetBadgeToDriver :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> FleetBadge -> Flow ()
linkFleetBadgeToDriver driverId _ _ fleetOwnerId badge = do
  createBadgeAssociation -- Upsert the badge association
  QP.updatePersonName driverId badge.badgeName
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
          createdAt = now,
          driverId = driverId,
          fleetOwnerId = fleetOwnerId,
          id = Id fleetBadgeId,
          isActive = True,
          updatedAt = now
        }

linkVehicleToDriver :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Text -> VehicleRegistrationCertificate -> Flow ()
linkVehicleToDriver driverId merchantId merchantOperatingCityId _ vehicleNumber vehicleRC = do
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
      driverRCAssoc <- makeRCAssociation merchantId merchantOperatingCityId driverId vehicleRC.id (DomainRC.convertTextToUTC (Just "2099-12-12"))
      DAQuery.create driverRCAssoc

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

-- QP.updatePersonName driverId "Driver"

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

findNextActiveTripTransaction :: Id Person -> Flow (Maybe TripTransaction)
findNextActiveTripTransaction driverId = do
  findNextEligibleTripTransactionByDriverIdStatus driverId IN_PROGRESS
    |<|>| findNextEligibleTripTransactionByDriverIdStatus driverId TRIP_ASSIGNED

-- Redis Transaction Lock Keys --
tripTransactionKey :: Id Person -> TripStatus -> Text
tripTransactionKey driverId = \case
  TRIP_ASSIGNED -> "WMB:TA:" <> driverId.getId
  IN_PROGRESS -> "WMB:TI:" <> driverId.getId
  COMPLETED -> "WMB:TCO:" <> driverId.getId
  CANCELLED -> "WMB:TCA:" <> driverId.getId
  PAUSED -> "WMB:TP:" <> driverId.getId

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
            routeCode = tripTransaction.routeCode,
            alertRequestType = alertRequestType,
            isViolated = isViolated,
            createdAt = now,
            updatedAt = now,
            merchantId = tripTransaction.merchantId,
            merchantOperatingCityId = tripTransaction.merchantOperatingCityId
          }
      TN.notifyWithGRPCProvider tripTransaction.merchantOperatingCityId Notification.TRIGGER_FCM title body driverId requestData
      pure alertRequest.id
    else do
      tripAlertRequest <- QTAR.findLatestTripAlertRequest tripTransaction.merchantOperatingCityId tripTransaction.fleetOwnerId.getId alertRequestType driverId.getId tripTransaction.routeCode >>= fromMaybeM (TripAlertRequestNotFound tripTransaction.id.getId)
      QTAR.updateIsViolated False tripAlertRequest.id
      pure tripAlertRequest.alertRequestId
