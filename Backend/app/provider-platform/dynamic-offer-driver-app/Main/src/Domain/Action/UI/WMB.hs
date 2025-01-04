{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( postWmbTripRequest,
    postWmbRequestsCancel,
    driverRequestLockKey,
    getWmbAvailableRoutes,
    postWmbTripLink,
    getWmbTripActive,
    postWmbTripStart,
    postWmbTripEnd,
    getWmbTripList,
  )
where

import API.Types.UI.WMB
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.List (minimumBy, sortBy)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.Call as Call
import Domain.Types.ApprovalRequest
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.Common
import Domain.Types.Extra.TransporterConfig
import Domain.Types.FleetDriverAssociation
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import Domain.Types.Route
import Domain.Types.RouteTripStopMapping
import Domain.Types.TripTransaction
import Domain.Types.Vehicle
import Domain.Types.VehicleVariant
import Environment
import qualified EulerHS.Prelude as EHS
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Types hiding (fromList)
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Queries.ApprovalRequest as QDR
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDA
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.RouteTripStopMapping as QRTS
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
import qualified Storage.Queries.TransporterConfig as QTC
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.TripTransactionExtra as QTTE
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VehicleRouteMapping as VRM
import qualified Storage.Queries.VehicleRouteMappingExtra as VRME
import qualified Tools.Call as Call
import Tools.Error
import qualified Tools.Notifications as TN

checkFleetDriverAssociation :: Id Person -> Flow Bool
checkFleetDriverAssociation driverId = do
  val <- FDA.findByDriverId driverId True
  case val of
    Nothing -> return False
    Just info -> return info.isActive

utctTimeToDayOfWeek :: Kernel.Prelude.UTCTime -> DayOfWeek
utctTimeToDayOfWeek utcTime = dayOfWeek (utctDay utcTime)

getSourceAndDestinationStopInfo :: Domain.Types.Route.Route -> Text -> Flow (StopInfo, StopInfo)
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

upsertFleetDriverAssociation :: Id Person -> Id Person -> Flow ()
upsertFleetDriverAssociation driverId fleetOwnerId = do
  val <- FDA.findByDriverId driverId True
  case val of
    Nothing -> do
      now <- getCurrentTime
      id <- generateGUID
      let fleetDriverAssc =
            FleetDriverAssociation
              { associatedTill = Nothing,
                driverId = driverId,
                fleetOwnerId = fleetOwnerId.getId,
                isActive = True,
                associatedOn = Just now,
                createdAt = now,
                updatedAt = now,
                ..
              }
      FDA.create fleetDriverAssc
      return ()
    Just _ -> return ()

availableRoutes :: (Text, ServiceTierType) -> Text -> Flow AvailableRoute
availableRoutes (routeCode, vehicleServiceTierType) vhclNo = do
  route <- QR.findByRouteCode routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route routeCode
  pure $
    AvailableRoute
      { routeInfo = buildRouteInfo route,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        vehicleDetails =
          VehicleDetails
            { number = vhclNo,
              _type = vehicleServiceTierType
            }
      }

getWmbAvailableRoutes ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.AvailableRouteReq ->
    Flow [API.Types.UI.WMB.AvailableRoute]
  )
getWmbAvailableRoutes (_, _, _) req = do
  -- -- HACK TO MAKE EXISTING DbHash work, as it internally does encodeHex (in ToJSON) defore doing db query.
  vehicleNumberHash <-
    case (A.fromJSON $ A.String req.vehicleNumber :: A.Result DbHash) of
      A.Success vehicleNumberHash -> pure vehicleNumberHash
      A.Error err -> throwError $ InternalError (T.pack err)

  result <-
    mapM
      ( \mapping -> do
          vehicleNumber <- decrypt mapping.vehicleNumber
          availableRoutes (mapping.routeCode, mapping.vehicleServiceTierType) vehicleNumber
      )
      =<< VRME.findAllRouteMappings vehicleNumberHash
  pure result

postWmbTripLink ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripLinkReq ->
    Flow API.Types.UI.WMB.TripTransactionDetails
  )
postWmbTripLink (mbDriverId, merchantId, merchantOperatingCityId) req = do
  driverId <- fromMaybeM (InvalidRequest "Driver ID not found") mbDriverId
  vehicleNumberHash <-
    case (A.fromJSON $ A.String req.vehicleNumberHash :: A.Result DbHash) of
      A.Success vehicleNumberHash -> pure vehicleNumberHash
      A.Error err -> throwError $ InternalError (T.pack err)
  vehicleRouteMapping <- VRME.findOneMapping vehicleNumberHash req.routeCode >>= fromMaybeM (InvalidRequest "Vehicle Route mapping not found")
  route <- QR.findByRouteCode req.routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  fleetConfig <- QFC.findByPrimaryKey vehicleRouteMapping.fleetOwnerId >>= fromMaybeM (InvalidRequest "Fleet Config Info not found")
  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route req.routeCode
  vehicleNumber <- decrypt vehicleRouteMapping.vehicleNumber
  curVehicleDetails <- QV.findByRegistrationNo vehicleNumber
  case curVehicleDetails of
    Just x | x.driverId /= driverId -> throwError (InvalidRequest "Already Linked")
    _ -> pure ()
  upsertFleetDriverAssociation driverId vehicleRouteMapping.fleetOwnerId
  tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus driverId
  case tripTransactions of
    [] -> pure ()
    _ -> throwError (InvalidRequest "Could not link")
  tripTransactionId <- generateGUID
  now <- getCurrentTime
  QV.create $ buildVehicle driverId vehicleRouteMapping vehicleNumber now
  QTT.create $ buildTripTransaction tripTransactionId driverId vehicleRouteMapping fleetConfig destinationStopInfo.code vehicleNumber now
  QDI.updateOnRide True driverId
  let busTripInfo = buildBusTripInfo vehicleNumber req.routeCode destinationStopInfo.point
  void $ LF.rideDetails (cast tripTransactionId) DRide.NEW merchantId driverId req.location.lat req.location.lon Nothing (Just busTripInfo)
  let tripAssignedEntityData = buildTripAssignedData tripTransactionId route vehicleRouteMapping vehicleNumber
  TN.notifyWmbOnRide driverId merchantOperatingCityId TRIP_ASSIGNED "Ride Assigned" "You have been assigned a ride" (Just tripAssignedEntityData)
  pure $
    TripTransactionDetails
      { tripTransactionId = tripTransactionId,
        vehicleNum = vehicleNumber,
        vehicleType = vehicleRouteMapping.vehicleServiceTierType,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        status = TRIP_ASSIGNED,
        routeInfo = buildRouteInfo route
      }
  where
    buildVehicle driverId vehicleRouteMapping vehicleNumber now =
      Vehicle
        { airConditioned = Nothing,
          capacity = Nothing,
          category = Nothing,
          color = vehicleRouteMapping.vehicleColor,
          downgradeReason = Nothing,
          driverId = driverId,
          energyType = Nothing,
          luggageCapacity = Nothing,
          mYManufacturing = Nothing,
          make = Nothing,
          merchantId = merchantId,
          model = vehicleRouteMapping.vehicleModel,
          oxygen = Nothing,
          registrationCategory = Nothing,
          registrationNo = vehicleNumber,
          selectedServiceTiers = vehicleRouteMapping.vehicleServiceTierType : [],
          size = Nothing,
          variant = castServiceTierToVariant vehicleRouteMapping.vehicleServiceTierType,
          vehicleClass = vehicleRouteMapping.vehicleClass,
          vehicleName = Nothing,
          vehicleRating = Nothing,
          ventilator = Nothing,
          merchantOperatingCityId = Just merchantOperatingCityId,
          createdAt = now,
          updatedAt = now
        }

    buildTripAssignedData tripTransactionId route vehicleRouteMapping vehicleNumber =
      TN.WMBTripAssignedData
        { tripTransactionId = tripTransactionId,
          routeCode = req.routeCode,
          routeShortname = route.shortName,
          vehicleNumber = vehicleNumber,
          vehicleServiceTierType = vehicleRouteMapping.vehicleServiceTierType,
          roundRouteCode = route.roundRouteCode
        }

    buildTripTransaction tripTransactionId driverId vehicleRouteMapping fleetConfig endStopCode vehicleNumber now =
      TripTransaction
        { allowEndingMidRoute = fleetConfig.allowEndingMidRoute,
          deviationCount = 0,
          driverId = driverId,
          endLocation = Nothing,
          fleetOwnerId = vehicleRouteMapping.fleetOwnerId,
          id = tripTransactionId,
          isCurrentlyDeviated = False,
          routeCode = req.routeCode,
          status = TRIP_ASSIGNED,
          vehicleNumber = vehicleNumber,
          startLocation = Nothing,
          startedNearStopCode = Nothing,
          tripCode = Nothing,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          createdAt = now,
          updatedAt = now,
          sequenceNumber = 0,
          ..
        }

getTripTransactionDetails ::
  Domain.Types.TripTransaction.TripTransaction ->
  Flow TripTransactionDetails
getTripTransactionDetails tripTransaction = do
  vehicleNumberHash <- getDbHash tripTransaction.vehicleNumber
  vehicleRouteMapping <- VRME.findOneMapping vehicleNumberHash tripTransaction.routeCode >>= fromMaybeM (InvalidRequest "Vehicle Route mapping not found")
  route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route tripTransaction.routeCode
  return $
    TripTransactionDetails
      { tripTransactionId = tripTransaction.id,
        vehicleNum = tripTransaction.vehicleNumber,
        vehicleType = vehicleRouteMapping.vehicleServiceTierType,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        status = tripTransaction.status,
        routeInfo = buildRouteInfo route
      }

getWmbTripActive ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow API.Types.UI.WMB.ActiveTripTransaction
  )
getWmbTripActive (mbDriverId, _, _) = do
  driverId <- fromMaybeM (InternalError "Driver id not found") mbDriverId
  checkFleetDriverAssociation driverId >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  findNextEligibleTripTransactionByDriverIdStatus driverId IN_PROGRESS
    |<|>| findNextEligibleTripTransactionByDriverIdStatus driverId TRIP_ASSIGNED
      >>= \case
        Nothing -> pure $ API.Types.UI.WMB.ActiveTripTransaction Nothing
        Just tripTransaction -> do
          tripTransactionDetails <- getTripTransactionDetails tripTransaction
          pure $ API.Types.UI.WMB.ActiveTripTransaction (Just tripTransactionDetails)

findClosestStop :: LatLong -> [(Text, LatLong)] -> Maybe Text
findClosestStop loc stops =
  if null stops
    then Nothing
    else Just $ fst $ minimumBy (EHS.comparing (KU.distanceBetweenInMeters loc . snd)) stops

postWmbTripStart ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id TripTransaction ->
    API.Types.UI.WMB.TripStartReq ->
    Flow Kernel.Types.APISuccess.APISuccess
  )
postWmbTripStart (_, _, _) tripTransactionId req = do
  tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (InternalError "no trip transaction found")
  checkFleetDriverAssociation tripTransaction.driverId >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  allStops <- QRTSM.findByRouteCode tripTransaction.routeCode
  let stops = map (\stop -> (stop.tripCode, stop.stopPoint)) allStops
  let closestStopTripCode = findClosestStop req.location stops
  case closestStopTripCode of
    Just tripCode -> do
      tripEndStop <- (listToMaybe $ sortBy (EHS.comparing (.stopSequenceNum)) $ filter (\stop -> stop.tripCode == tripCode) allStops) & fromMaybeM (InternalError "End Stop Not Found.")
      let busTripInfo = buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode tripEndStop.stopPoint
      void $ LF.rideStart (cast tripTransaction.id) req.location.lat req.location.lon tripTransaction.merchantId tripTransaction.driverId (Just busTripInfo)
      QTT.updateOnStart tripCode (Just req.location) IN_PROGRESS tripTransactionId
      TN.notifyWmbOnRide tripTransaction.driverId tripTransaction.merchantOperatingCityId IN_PROGRESS "Ride Started" "Your ride has started" Nothing
      pure Success
    Nothing -> throwError (InvalidRequest "Could not start trip")

postWmbTripEnd ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id TripTransaction ->
    API.Types.UI.WMB.TripEndReq ->
    Flow API.Types.UI.WMB.TripEndResp
  )
postWmbTripEnd (person, _, merchantOpCityId) tripTransactionId req = do
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  fleetDriverAssociation <- FDA.findByDriverId driverId True >>= fromMaybeM (InternalError "Fleet Association not found")
  tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (InternalError "no trip transaction found")
  routeInfo <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (InternalError "Route not found")
  fleetConfigInfo <- QFC.findByPrimaryKey (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (InternalError "Fleet Config Info not found")
  let distanceLeft = KU.distanceBetweenInMeters req.location routeInfo.endPoint
      rideEndEligibility = fleetConfigInfo.allowEndingMidRoute || distanceLeft <= fleetConfigInfo.endRideDistanceThreshold
  unless rideEndEligibility $
    throwError $ InvalidRequest "Ride Not Eligible For Ending"
  if fleetConfigInfo.rideEndApproval
    then do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      driverMobileNumber <- mapM decrypt driver.mobileNumber
      let requestData =
            EndRide
              EndRideData
                { lat = req.location.lat,
                  lon = req.location.lon,
                  tripTransactionId = tripTransaction.id,
                  vehicleRegistrationNumber = tripTransaction.vehicleNumber,
                  driverName = driver.firstName <> " " <> (fromMaybe "" driver.lastName),
                  driverMobileNumber = driverMobileNumber,
                  tripCode = tripTransaction.tripCode
                }
          requestTitle = "Your trip has ended!"
          requestBody = "You reached your end stop of this route."
      driverReq <- createDriverRequest driverId fleetDriverAssociation.fleetOwnerId requestTitle requestBody requestData tripTransaction
      pure $ TripEndResp {requestId = Just driverReq.id.getId, result = WAITING_FOR_ADMIN_APPROVAL}
    else do
      advancedTripTransaction <- findNextEligibleTripTransactionByDriverIdStatus driverId TRIP_ASSIGNED
      void $ LF.rideEnd (cast tripTransaction.id) req.location.lat req.location.lon tripTransaction.merchantId driverId (advancedTripTransaction <&> (cast . (.id)))
      QDI.updateOnRide False driverId
      QTT.updateStatus COMPLETED (Just routeInfo.endPoint) tripTransactionId
      QV.deleteByDriverid driverId
      TN.notifyWmbOnRide driverId merchantOpCityId COMPLETED "Ride Ended" "Your ride has ended" Nothing
      pure $ TripEndResp {requestId = Nothing, result = SUCCESS}

getWmbTripList ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Maybe (Int) ->
    Maybe (Int) ->
    Maybe (Domain.Types.TripTransaction.TripStatus) ->
    Flow [API.Types.UI.WMB.TripTransactionDetails]
  )
getWmbTripList (mbDriverId, _, _) limit offset status = do
  driverId <- fromMaybeM (InternalError "Driver id not found") mbDriverId
  checkFleetDriverAssociation driverId >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  allTripTransactionDriver <- QTTE.findAllTripTransactionByDriverIdStatus driverId limit offset status
  tripTransactions <- mapM getTripTransactionDetails allTripTransactionDriver
  pure tripTransactions

postWmbRequestsCancel ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id ApprovalRequest ->
    Flow APISuccess
  )
postWmbRequestsCancel (mbPersonId, _, _) approvalRequestId = do
  Redis.whenWithLockRedis (driverRequestLockKey approvalRequestId.getId) 60 $ do
    approvalRequest <- QDR.findByPrimaryKey approvalRequestId >>= fromMaybeM (InvalidRequest "Approval Request Id not found")
    case mbPersonId of
      Just personId -> unless (approvalRequest.requestorId == personId) $ throwError NotAnExecutor
      _ -> pure ()
    unless (approvalRequest.status == AWAITING_APPROVAL) $ throwError (InvalidRequest "Request already processed")
    QDR.updateStatusWithReason REVOKED (Just "Cancelled by driver") approvalRequestId
  pure Success

postWmbTripRequest ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id TripTransaction ->
    RequestDetails ->
    Flow DriverReqResp
  )
postWmbTripRequest (_, merchantId, merchantOperatingCityId) tripTransactionId req = do
  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  tripTransaction <- QTT.findByPrimaryKey tripTransactionId >>= fromMaybeM (InvalidRequest "Invalid trip transaction id")
  awaitingRequests <- QDR.findByStatus tripTransaction.driverId AWAITING_APPROVAL
  when (isJust $ find (\awaitingRequest -> getRequestType awaitingRequest.requestData == getRequestType req.requestData) awaitingRequests) $ throwError (InvalidRequest "Duplicate request")
  fleetDriverAssociation <- FDV.findByDriverId tripTransaction.driverId True >>= fromMaybeM (InvalidRequest "Driver is not part of this fleet")
  driverReq <- createDriverRequest tripTransaction.driverId fleetDriverAssociation.fleetOwnerId req.title req.body req.requestData tripTransaction
  let maybeAppId = (HM.lookup FleetAppletID . exotelMap) =<< transporterConfig.exotelAppIdMapping -- currently only for END_RIDE
  whenJust transporterConfig.fleetAlertThreshold $ \threshold -> do
    let triggerFleetAlertTs = secondsToNominalDiffTime threshold
    createJobIn @_ @'FleetAlert (Just merchantId) (Just merchantOperatingCityId) triggerFleetAlertTs $
      FleetAlertJobData
        { fleetOwnerId = Id fleetDriverAssociation.fleetOwnerId,
          entityId = driverReq.id,
          appletId = maybeAppId
        }
  pure $ DriverReqResp {requestId = driverReq.id.getId}
  where
    getRequestType = \case
      EndRide _ -> END_RIDE
      ChangeRoute _ -> CHANGE_ROUTE

driverRequestLockKey :: Text -> Text
driverRequestLockKey reqId = "Driver:Request:Id-" <> reqId

createDriverRequest :: Id Person -> Text -> Text -> Text -> ApprovalRequestData -> TripTransaction -> Flow ApprovalRequest
createDriverRequest driverId requesteeId title body requestData tripTransaction = do
  id <- generateGUID
  now <- getCurrentTime
  let driverReq =
        ApprovalRequest
          { requestorId = driverId,
            requesteeId = Id requesteeId,
            reason = Nothing,
            status = AWAITING_APPROVAL,
            createdAt = now,
            updatedAt = now,
            merchantId = tripTransaction.merchantId,
            merchantOperatingCityId = tripTransaction.merchantOperatingCityId,
            ..
          }
  QDR.create driverReq
  TN.notifyWithGRPCProvider tripTransaction.merchantOperatingCityId Notification.TRIGGER_FCM title body driverId requestData
  pure driverReq

buildBusTripInfo :: Text -> Text -> LatLong -> LT.RideInfo
buildBusTripInfo vehicleNumber routeCode destinationLocation =
  LT.Bus
    { busNumber = vehicleNumber,
      destination = destinationLocation,
      ..
    }

findNextEligibleTripTransactionByDriverIdStatus :: Id Person -> TripStatus -> Flow (Maybe TripTransaction)
findNextEligibleTripTransactionByDriverIdStatus driverId status =
  QTTE.findAllTripTransactionByDriverIdStatus driverId (Just 1) (Just 0) (Just status) >>= \case
    (trip : _) -> pure (Just trip)
    [] -> pure Nothing

buildRouteInfo :: Route -> RouteInfo
buildRouteInfo Route {..} = RouteInfo {..}
