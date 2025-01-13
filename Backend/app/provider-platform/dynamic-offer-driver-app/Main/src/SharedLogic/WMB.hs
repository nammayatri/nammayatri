module SharedLogic.WMB where

import API.Types.UI.WMB
import Data.Time hiding (getCurrentTime)
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
import Environment
import qualified EulerHS.Prelude as EHS
import Kernel.External.Encryption (getDbHash)
import Kernel.External.Maps
import Kernel.Prelude
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
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
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

findClosestStop :: LatLong -> [StopData] -> Maybe StopData
findClosestStop loc stops =
  if null stops
    then Nothing
    else Just $ minimumBy (EHS.comparing (KU.distanceBetweenInMeters loc . stopLocation)) stops

checkFleetDriverAssociation :: Id Person -> Id Person -> Flow Bool
checkFleetDriverAssociation driverId fleetOwnerId = do
  mbFleetDriverAssociation <- QFDV.findByDriverId driverId True
  case mbFleetDriverAssociation of
    Nothing -> return False
    Just fleetDriverAssociation -> return $ fleetDriverAssociation.isActive && fleetDriverAssociation.fleetOwnerId == fleetOwnerId.getId

getSourceAndDestinationStopInfo :: Route -> Text -> Flow (StopInfo, StopInfo)
getSourceAndDestinationStopInfo route routeCode = do
  now <- getCurrentTime
  allRTSList <- QRTS.findAllRTSMappingByRouteAndDay routeCode (utctTimeToDayOfWeek now)
  nonEmptyAllRTSList <-
    case allRTSList of
      [] -> throwError $ InvalidRequest "RTS not found"
      (a : as) -> pure $ a :| as
  let sourceRouteTripMapping = minimumBy (EHS.comparing (\r -> KU.distanceBetweenInMeters route.startPoint r.stopPoint)) nonEmptyAllRTSList
      destinationRouteTripMapping = minimumBy (EHS.comparing (\r -> KU.distanceBetweenInMeters route.endPoint r.stopPoint)) nonEmptyAllRTSList
  pure (createStopInfo sourceRouteTripMapping route.startPoint, createStopInfo destinationRouteTripMapping route.endPoint)
  where
    createStopInfo routeTripMapping point =
      StopInfo
        { name = routeTripMapping.stopName,
          code = routeTripMapping.stopCode,
          ..
        }

    utctTimeToDayOfWeek :: UTCTime -> DayOfWeek
    utctTimeToDayOfWeek utcTime = dayOfWeek (utctDay utcTime)

findNextEligibleTripTransactionByDriverIdStatus :: Id Person -> TripStatus -> Flow (Maybe TripTransaction)
findNextEligibleTripTransactionByDriverIdStatus driverId status = do
  let sortType = if isNonTerminalTripStatus status then QTT.SortAsc else QTT.SortDesc
  QTT.findAllTripTransactionByDriverIdStatus driverId (Just 1) (Just 0) (Just status) sortType >>= \case
    (trip : _) -> pure (Just trip)
    [] -> pure Nothing

isNonTerminalTripStatus :: TripStatus -> Bool
isNonTerminalTripStatus status = any (\status' -> status' == status) [TRIP_ASSIGNED, IN_PROGRESS]

buildTripAssignedData :: Id TripTransaction -> ServiceTierType -> Text -> Text -> Text -> (Maybe Text) -> TN.WMBTripAssignedData
buildTripAssignedData tripTransactionId vehicleServiceTier vehicleNumber routeCode shortName roundRouteCode =
  TN.WMBTripAssignedData
    { tripTransactionId = tripTransactionId,
      routeCode = routeCode,
      routeShortname = shortName,
      vehicleNumber = vehicleNumber,
      vehicleServiceTierType = vehicleServiceTier,
      roundRouteCode = roundRouteCode
    }

assignAndStartTripTransaction :: FleetConfig -> Id Merchant -> Id MerchantOperatingCity -> Id Person -> Route -> VehicleRouteMapping -> Text -> StopInfo -> LatLong -> Flow TripTransaction
assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber destinationStopInfo currentLocation = do
  tripTransactionId <- generateGUID
  now <- getCurrentTime
  allStops <- QRTSM.findByRouteCode route.code
  let stops = map (\stop -> StopData stop.tripCode stop.routeCode stop.stopCode stop.stopPoint) allStops
  closestStop <- findClosestStop currentLocation stops & fromMaybeM (InternalError "No closest stop found")
  vehicleRegistrationCertificate <- linkVehicleToDriver driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId vehicleNumber
  let tripTransaction = buildTripTransaction tripTransactionId destinationStopInfo.code now closestStop vehicleRegistrationCertificate
  QTT.create tripTransaction
  QDI.updateOnRide True driverId
  let busTripInfo = buildBusTripInfo vehicleNumber route.code destinationStopInfo.point
  void $ LF.rideDetails (cast tripTransactionId) DRide.NEW merchantId driverId currentLocation.lat currentLocation.lon Nothing (Just busTripInfo)
  void $ LF.rideStart (cast tripTransactionId) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId (Just busTripInfo)
  TN.notifyWmbOnRide driverId merchantOperatingCityId IN_PROGRESS "Ride Started" "You ride has started" EmptyDynamicParam
  return tripTransaction
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
          status = IN_PROGRESS,
          startLocation = (Just currentLocation),
          startedNearStopCode = (Just closestStop.stopCode),
          tripCode = (Just closestStop.tripCode),
          vehicleServiceTierType = maybe BUS_NON_AC DVehVariant.castVariantToServiceTier vehicleRegistrationCertificate.vehicleVariant,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          createdAt = now,
          updatedAt = now,
          tripStartTime = Nothing,
          tripEndTime = Nothing,
          ..
        }

endTripTransaction :: FleetConfig -> TripTransaction -> LatLong -> Flow ()
endTripTransaction fleetConfig tripTransaction currentLocation = do
  now <- getCurrentTime
  void $ LF.rideEnd (cast tripTransaction.id) currentLocation.lat currentLocation.lon tripTransaction.merchantId tripTransaction.driverId Nothing
  QDI.updateOnRide False tripTransaction.driverId
  QTT.updateOnEnd COMPLETED (Just currentLocation) (Just now) tripTransaction.id
  TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId COMPLETED "Ride Ended" "Your ride has ended" EmptyDynamicParam
  findNextEligibleTripTransactionByDriverIdStatus tripTransaction.driverId TRIP_ASSIGNED >>= \case
    Just advancedTripTransaction -> do
      route <- QR.findByRouteCode advancedTripTransaction.routeCode >>= fromMaybeM (InvalidRequest "Route not found")
      (_, destinationStopInfo) <- getSourceAndDestinationStopInfo route advancedTripTransaction.routeCode
      assignTripTransaction advancedTripTransaction route currentLocation destinationStopInfo.point
    Nothing -> do
      if fleetConfig.allowAutomaticRoundTripAssignment
        then do
          whenJust tripTransaction.roundRouteCode $ \roundRouteCode -> do
            route <- QR.findByRouteCode roundRouteCode >>= fromMaybeM (InvalidRequest "Route not found")
            (_, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
            vehicleNumberHash <- getDbHash tripTransaction.vehicleNumber
            vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash roundRouteCode >>= fromMaybeM (InvalidRequest "Vehicle Route mapping not found")
            void $ assignAndStartTripTransaction fleetConfig tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.driverId route vehicleRouteMapping tripTransaction.vehicleNumber destinationStopInfo currentLocation
        else unlinkVehicleToDriver fleetConfig tripTransaction.driverId tripTransaction.merchantId tripTransaction.merchantOperatingCityId tripTransaction.vehicleNumber

buildBusTripInfo :: Text -> Text -> LatLong -> LT.RideInfo
buildBusTripInfo vehicleNumber routeCode destinationLocation =
  LT.Bus
    { busNumber = vehicleNumber,
      destination = destinationLocation,
      ..
    }

assignTripTransaction :: TripTransaction -> Route -> LatLong -> LatLong -> Flow ()
assignTripTransaction tripTransaction route currentLocation destination = do
  let busTripInfo = buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode destination
  void $ LF.rideDetails (cast tripTransaction.id) DRide.NEW tripTransaction.merchantId tripTransaction.driverId currentLocation.lat currentLocation.lon Nothing (Just busTripInfo)
  let tripAssignedEntityData = buildTripAssignedData tripTransaction.id tripTransaction.vehicleServiceTierType tripTransaction.vehicleNumber tripTransaction.routeCode route.shortName route.roundRouteCode
  TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId TRIP_ASSIGNED "Ride Assigned" "Ride assigned" tripAssignedEntityData

linkVehicleToDriver :: Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Text -> Flow VehicleRegistrationCertificate
linkVehicleToDriver driverId merchantId merchantOperatingCityId fleetOwnerId vehicleNumber = do
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNumber >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId)
  unless (vehicleRC.verificationStatus == Documents.VALID) $ throwError (InvalidRequest "Cannot link to driver because Rc is not valid")
  QV.findByRegistrationNo vehicleNumber >>= \case
    Just vehicle -> when (vehicle.driverId /= driverId) $ throwError (InvalidRequest "Vehicle is linked to some other driver, first unlink then try")
    Nothing -> pure ()
  tryLinkinRC vehicleRC
  QDI.updateEnabledVerifiedState driverId True (Just True)
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

unlinkVehicleToDriver :: FleetConfig -> Id Person -> Id Merchant -> Id MerchantOperatingCity -> Text -> Flow ()
unlinkVehicleToDriver fleetConfig driverId merchantId merchantOperatingCityId vehicleNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = vehicleNumber,
            isActivate = False
          }
  void $ DomainRC.linkRCStatus (driverId, merchantId, merchantOperatingCityId) rcStatusReq
  when (not fleetConfig.allowStartRideFromQR) $ QDI.updateEnabledVerifiedState driverId False (Just False)
