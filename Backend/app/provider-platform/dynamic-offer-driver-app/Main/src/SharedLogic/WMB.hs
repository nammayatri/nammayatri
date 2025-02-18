module SharedLogic.WMB where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
import API.Types.UI.WMB
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.Common
import Domain.Types.EmptyDynamicParam
import Domain.Types.FleetConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import Domain.Types.Route
import Domain.Types.TripTransaction
import Domain.Types.VehicleRegistrationCertificate
import Domain.Types.VehicleRouteMapping
import qualified Domain.Types.VehicleVariant as DVehVariant
import Domain.Utils
import Environment
import qualified EulerHS.Prelude as EHS
import Kernel.External.Encryption (getDbHash)
import Kernel.External.Maps
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.RouteTripStopMapping as QRTS
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

assignAndStartTripTransaction :: FleetConfig -> Id Merchant -> Id MerchantOperatingCity -> Id Person -> Route -> VehicleRouteMapping -> Text -> StopInfo -> LatLong -> Flow TripTransaction
assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber destinationStopInfo currentLocation = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey driverId TRIP_ASSIGNED <> tripTransactionKey driverId IN_PROGRESS)
    60
    ( do
        tripTransactionId <- generateGUID
        now <- getCurrentTime
        closestStop <- findClosestStop route.code currentLocation >>= fromMaybeM (StopNotFound)
        vehicleRegistrationCertificate <- linkVehicleToDriver driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId vehicleNumber
        let tripTransaction = buildTripTransaction tripTransactionId destinationStopInfo.code now closestStop vehicleRegistrationCertificate
        -- TODO :: Handle Transaction Failure
        QTT.create tripTransaction
        assignTripTransaction tripTransaction route False currentLocation destinationStopInfo.point False
        startTripTransaction tripTransaction route closestStop currentLocation destinationStopInfo.point True
    )
    >>= \case
      Right tripTransaction -> return tripTransaction
      Left _ -> throwError (InternalError "Process for Trip Assignment & Start is Already Ongoing, Please try again!")
  where
    buildTripTransaction tripTransactionId endStopCode now closestStop vehicleRegistrationCertificate =
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
          vehicleServiceTierType = maybe BUS_NON_AC DVehVariant.castVariantToServiceTier vehicleRegistrationCertificate.vehicleVariant,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          createdAt = now,
          updatedAt = now,
          endRideApprovalRequestId = Nothing,
          tripStartTime = Just now,
          tripEndTime = Nothing,
          tripTerminationSource = Nothing,
          ..
        }

endTripTransaction :: FleetConfig -> TripTransaction -> LatLong -> ActionSource -> Flow ()
endTripTransaction fleetConfig tripTransaction currentLocation tripTerminationSource = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId COMPLETED)
    60
    ( do
        unless (tripTransaction.status == IN_PROGRESS) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        now <- getCurrentTime
        void $ LF.rideEnd (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId Nothing
        QTT.updateOnEnd COMPLETED (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
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
                  (_, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
                  vehicleNumberHash <- getDbHash tripTransaction.vehicleNumber
                  vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash roundRouteCode >>= fromMaybeM (VehicleRouteMappingNotFound tripTransaction.vehicleNumber roundRouteCode)
                  when (not vehicleRouteMapping.blocked) $ do
                    void $ assignAndStartTripTransaction fleetConfig tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.driverId route vehicleRouteMapping tripTransaction.vehicleNumber destinationStopInfo currentLocation
              else do
                QDI.updateOnRide False tripTransaction.driverId
                unlinkVehicleToDriver tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber
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
                      findNextEligibleTripTransactionByDriverIdStatus tripTransaction.driverId TRIP_ASSIGNED >>= \case
                        Just advancedTripTransaction -> do
                          route <- QR.findByRouteCode advancedTripTransaction.routeCode >>= fromMaybeM (RouteNotFound advancedTripTransaction.routeCode)
                          (_, destinationStopInfo) <- getSourceAndDestinationStopInfo route advancedTripTransaction.routeCode
                          assignTripTransaction advancedTripTransaction route False currentLocation destinationStopInfo.point True
                        Nothing -> do
                          QDI.updateOnRide False tripTransaction.driverId
                          unlinkVehicleToDriver tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber
                    IN_PROGRESS -> endTripTransaction fleetConfig tripTransaction currentLocation tripTerminationSource
                    _ -> pure ()
                else do
                  now <- getCurrentTime
                  QTT.updateOnEnd CANCELLED (Just currentLocation) (Just now) (Just tripTerminationSource) tripTransaction.id
    )
    >>= \case
      Right _ -> return ()
      Left _ -> throwError (InternalError "Process for Trip Cancellation is Already Ongoing, Please try again!")

buildBusTripInfo :: Text -> Text -> LatLong -> LT.RideInfo
buildBusTripInfo vehicleNumber routeCode destinationLocation =
  LT.Bus
    { busNumber = vehicleNumber,
      destination = destinationLocation,
      ..
    }

assignTripTransaction :: TripTransaction -> Route -> Bool -> LatLong -> LatLong -> Bool -> Flow ()
assignTripTransaction tripTransaction route isFirstBatchTrip currentLocation destination notify = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId TRIP_ASSIGNED)
    60
    ( do
        unless (tripTransaction.status == TRIP_ASSIGNED) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        let busTripInfo = buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode destination
        void $ LF.rideDetails (cast tripTransaction.id) DRide.NEW tripTransaction.merchantId tripTransaction.driverId currentLocation.lat currentLocation.lon Nothing (Just busTripInfo)
        QDI.updateOnRide True tripTransaction.driverId
        when notify $ do
          let tripAssignedEntityData = buildTripAssignedData tripTransaction.id tripTransaction.vehicleServiceTierType tripTransaction.vehicleNumber tripTransaction.routeCode route.shortName route.roundRouteCode isFirstBatchTrip
          TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId TRIP_ASSIGNED "Ride Assigned" "Ride assigned" tripAssignedEntityData
    )
    >>= \case
      Right _ -> return ()
      Left _ -> throwError (InternalError "Process for Trip Assignment is Already Ongoing, Please try again!")

startTripTransaction :: TripTransaction -> Route -> StopData -> LatLong -> LatLong -> Bool -> Flow TripTransaction
startTripTransaction tripTransaction _route closestStop currentLocation destination notify = do
  Hedis.whenWithLockRedisAndReturnValue
    (tripTransactionKey tripTransaction.driverId IN_PROGRESS)
    60
    ( do
        unless (tripTransaction.status == TRIP_ASSIGNED) $ throwError (InvalidTripStatus $ show tripTransaction.status)
        let busTripInfo = buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode destination
        void $ LF.rideStart (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId (Just busTripInfo)
        now <- getCurrentTime
        QDI.updateOnRide True tripTransaction.driverId
        let tripStartTransaction = tripTransaction {tripCode = Just closestStop.tripCode, startedNearStopCode = Just closestStop.stopCode, startLocation = Just currentLocation, status = IN_PROGRESS, tripStartTime = Just now}
        QTT.updateOnStart tripStartTransaction.tripCode tripStartTransaction.startedNearStopCode tripStartTransaction.startLocation tripStartTransaction.status tripStartTransaction.tripStartTime tripStartTransaction.id
        when notify $ do
          TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId IN_PROGRESS "Ride Started" "Your ride has started" EmptyDynamicParam
        return tripStartTransaction
    )
    >>= \case
      Right tripStartTransaction -> return tripStartTransaction
      Left _ -> throwError (InternalError "Process for Trip Start is Already Ongoing, Please try again!")

linkVehicleToDriver :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Text -> Flow VehicleRegistrationCertificate
linkVehicleToDriver driverId merchantId merchantOperatingCityId fleetOwnerId vehicleNumber = do
  findNextActiveTripTransaction driverId
    >>= \case
      Nothing -> pure ()
      Just tripTransaction -> unless (tripTransaction.vehicleNumber == vehicleNumber) $ throwError (AlreadyOnActiveTripWithAnotherVehicle tripTransaction.vehicleNumber)
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNumber >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId)
  unless (vehicleRC.verificationStatus == Documents.VALID) $ throwError (RcNotValid)
  QV.findByRegistrationNo vehicleNumber >>= \case
    Just vehicle -> when (vehicle.driverId /= driverId) $ throwError (VehicleLinkedToAnotherDriver vehicleNumber)
    Nothing -> pure ()
  tryLinkinRC vehicleRC
  return vehicleRC
  where
    tryLinkinRC vehicleRC = do
      now <- getCurrentTime
      mRCAssociation <- DAQuery.findLatestByRCIdAndDriverId vehicleRC.id driverId
      case mRCAssociation of
        Just assoc -> do
          when (maybe True (now >) assoc.associatedTill) $ -- if that association is old, create new association for that driver
            createRCAssociation vehicleRC
        Nothing -> createRCAssociation vehicleRC
      let rcStatusReq =
            DomainRC.RCStatusReq
              { rcNo = vehicleNumber,
                isActivate = True
              }
      void $ DomainRC.linkRCStatus (driverId, merchantId, merchantOperatingCityId) rcStatusReq
    createRCAssociation vehicleRC = do
      driverRCAssoc <- makeRCAssociation merchantId merchantOperatingCityId driverId vehicleRC.id (DomainRC.convertTextToUTC (Just "2099-12-12"))
      DAQuery.create driverRCAssoc

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
