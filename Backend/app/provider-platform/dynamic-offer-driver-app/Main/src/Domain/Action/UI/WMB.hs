{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( getUiWmbAvailableRoutes,
    postUiWmbTripLink,
    getUiWmbTripActive,
    postUiWmbTripStart,
    postUiWmbTripEnd,
    postUiWmbTripRequest,
    postUiWmbRequestsCancel,
    driverRequestLockKey,
    getUiWmbTripList,
  )
where

import API.Types.UI.WMB
import Data.List (maximum)
import Data.Maybe
import API.Types.UI.WMB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text
import qualified Domain.Action.UI.Call as Call
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.DriverRequest
import Domain.Types.Extra.TransporterConfig
import Domain.Types.ApprovalRequest
import Domain.Types.Common
import Domain.Types.FleetDriverAssociation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.TripTransaction
import Domain.Types.Vehicle
import Domain.Types.VehicleVariant
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, maximum)
import Kernel.External.Encryption
import Kernel.External.Maps.Types hiding (fromList)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import qualified Storage.Queries.ApprovalRequest as QDR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDA
import qualified Storage.Queries.Route as QRM
import qualified Storage.Queries.RouteTripStopMapping as QRTS
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
import qualified Storage.Queries.TransporterConfig as QTC
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.TripTransactionExtra as QTTE
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VehicleRouteMapping as VRM
import qualified Storage.Queries.VehicleRouteMappingExtra as VRME
import qualified Tools.Notifications as TN

checkFleetDriverAssociation :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.Flow Bool
checkFleetDriverAssociation driverId = do
  case driverId of
    Nothing -> return False
    Just id -> do
      val <- FDA.findByDriverId id True
      case val of
        Nothing -> return False
        Just info -> return info.isActive

checkFleetDriverAssociationAndCreateIfNotExists :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow ()
checkFleetDriverAssociationAndCreateIfNotExists driverId fleetOwnerId = do
  case driverId of
    Nothing -> throwError $ InternalError "Could not create Fleet-driver association as Driver not found"
    Just id -> do
      val <- FDA.findByDriverId id True
      case val of
        Nothing -> do
          -- create fleet driver association
          now <- getCurrentTime
          fleetAsscId <- generateGUID
          let fleetDriverAssc =
                FleetDriverAssociation
                  { associatedOn = (Just now),
                    associatedTill = Nothing,
                    createdAt = now,
                    driverId = id,
                    fleetOwnerId = fleetOwnerId.getId,
                    id = fleetAsscId,
                    isActive = True,
                    updatedAt = now
                  }
          FDA.create fleetDriverAssc
          return ()
        Just _ -> return ()

availableRoutes :: (Text, ServiceTierType) -> Text -> Environment.Flow AvailableRoute
availableRoutes (routeCode, vehicleServiceTierType) vhclNo = do
  res <- QRM.findByRouteCode routeCode >>= fromMaybeM (InternalError "Route not found")
  infoSrc <- QRTS.findByLocation res.startPoint >>= fromMaybeM (InternalError "start point not found")
  infoDest <- QRTS.findByLocation res.endPoint >>= fromMaybeM (InternalError "end point not found")
  let routeData =
        RouteInfo
          { code = res.code,
            shortName = res.shortName,
            longName = res.longName,
            startPoint = res.startPoint,
            endPoint = res.endPoint
          }
  let vhclDetails =
        VehicleDetails
          { number = vhclNo,
            _type = vehicleServiceTierType
          }
  let srcInfo =
        StopInfo
          { name = infoSrc.stopName,
            code = infoSrc.stopCode,
            lat = Just res.startPoint.lat,
            long = Just res.startPoint.lon
          }
  let dstInfo =
        StopInfo
          { name = infoDest.stopName,
            code = infoDest.stopCode,
            lat = Just res.endPoint.lat,
            long = Just res.endPoint.lon
          }
  let availableRoutesList =
        AvailableRoute
          { routeInfo = routeData,
            source = srcInfo,
            destination = dstInfo,
            vehicleDetails = vhclDetails
          }
  pure availableRoutesList
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverRequest as QDR
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TripTransaction as QTT
import qualified Tools.Call as Call

getUiWmbAvailableRoutes ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.Flow [API.Types.UI.WMB.AvailableRoute]
  )
getUiWmbAvailableRoutes (_, _, _) vhclNumberHash = do
  let vehicleNumberHash = DbHash $ encodeUtf8 vhclNumberHash
  result <-
    mapM
      ( \mapping -> do
          vehicleNumber <- decrypt mapping.vehicleNumber
          availableRoutes (mapping.routeCode, mapping.vehicleServiceTierType) vehicleNumber
      )
      =<< VRME.findAllRouteMappings vehicleNumberHash
  pure result

postUiWmbTripLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripLinkReq ->
    Environment.Flow API.Types.UI.WMB.TripTransactionDetails
  )
postUiWmbTripLink (person, mer, city) obj = do
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  let vehicleNumberHash = DbHash $ encodeUtf8 obj.vehicleNumberHash
  mapping <- VRME.findOneMapping vehicleNumberHash obj.routeCode >>= fromMaybeM (InternalError "Vehicle Route mapping not found")
  vehicleNumber <- decrypt mapping.vehicleNumber
  curVehicleDetails <- QV.findByRegistrationNo vehicleNumber
  case curVehicleDetails of
    Just x | x.driverId /= driverId -> throwError (InternalError "Already Linked")
    _ -> pure ()

  checkFleetDriverAssociationAndCreateIfNotExists person mapping.fleetOwnerId
  fleetDriverAssociation <- FDA.findByDriverId driverId True >>= fromMaybeM (InternalError "Fleet Association not found")
  tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus driverId
  case tripTransactions of
    [] -> pure ()
    _ -> throwError (InternalError "Could not link")
  tripTransactionId <- generateGUID
  now <- getCurrentTime
  routeInfo <- QRM.findByRouteCode obj.routeCode >>= fromMaybeM (InternalError "Route not found")
  let createStopInfo nameCodeObj point =
        StopInfo
          { name = nameCodeObj.stopName,
            code = nameCodeObj.stopCode,
            lat = Just point.lat,
            long = Just point.lon
          }
  nameCodeObjStart <- QRTS.findByLocation routeInfo.startPoint >>= fromMaybeM (InternalError "No startPoint found")
  nameCodeObjEnd <- QRTS.findByLocation routeInfo.endPoint >>= fromMaybeM (InternalError "No endPoint found")
  fleetConfigInfo <- QFC.findByPrimaryKey (Kernel.Types.Id.Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (InternalError "Fleet Config Info not found")
  let source = createStopInfo nameCodeObjStart routeInfo.startPoint
  let destination = createStopInfo nameCodeObjEnd routeInfo.endPoint
  let result =
        TripTransactionDetails
          { tripTransactionId = tripTransactionId.getId,
            vehicleNum = vehicleNumber,
            vehicleType = mapping.vehicleServiceTierType,
            source = source,
            destination = destination
          }
  -- generate trip transaction
  let tripTransaction =
        TripTransaction
          { allowEndingMidRoute = fleetConfigInfo.allowEndingMidRoute,
            deviationCount = 0,
            driverId = driverId,
            endLocation = Nothing,
            endStopCode = nameCodeObjEnd.stopCode,
            fleetOwnerId = mapping.fleetOwnerId,
            id = tripTransactionId,
            isCurrentlyDeviated = False,
            routeCode = obj.routeCode,
            status = TRIP_ASSIGNED,
            vehicleNumber = vehicleNumber,
            startLocation = Nothing,
            startedNearStopCode = Nothing,
            tripCode = Nothing,
            merchantId = Nothing,
            merchantOperatingCityId = Just city,
            createdAt = now,
            updatedAt = now,
            sequenceNumber = 0
          }
  QTT.create tripTransaction
  QDI.updateOnRide True driverId
  let dataSend =
        TN.WMBTripAssignedData
          { tripTransactionId = tripTransactionId,
            routeCode = obj.routeCode,
            routeShortname = routeInfo.shortName,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = mapping.vehicleServiceTierType,
            roundRouteCode = routeInfo.roundRouteCode
          }
  TN.notifyWmbOnRide driverId city TRIP_ASSIGNED "Ride Assigned" "You have been assigned a ride" (Just dataSend)
  let variant = castServiceTierToVariant mapping.vehicleServiceTierType
  let vehicle =
        Vehicle
          { airConditioned = Nothing,
            capacity = Nothing,
            category = Nothing,
            color = mapping.vehicleColor,
            downgradeReason = Nothing,
            driverId = driverId,
            energyType = Nothing,
            luggageCapacity = Nothing,
            mYManufacturing = Nothing,
            make = Nothing,
            merchantId = mer,
            model = mapping.vehicleModel,
            oxygen = Nothing,
            registrationCategory = Nothing,
            registrationNo = vehicleNumber,
            selectedServiceTiers = mapping.vehicleServiceTierType : [],
            size = Nothing,
            variant = variant,
            vehicleClass = mapping.vehicleClass,
            vehicleName = Nothing,
            vehicleRating = Nothing,
            ventilator = Nothing,
            merchantOperatingCityId = (Just city),
            createdAt = now,
            updatedAt = now
          }
  QV.create vehicle
  pure result

getTripTransactionDetails ::
  Domain.Types.TripTransaction.TripTransaction ->
  Bool ->
  Environment.Flow (Either TripTransactionDetails TripTransactionDetailsExtra)
getTripTransactionDetails trip extra = do
  vehicleNumberHash <- getDbHash trip.vehicleNumber
  mapping <-
    VRME.findOneMapping vehicleNumberHash trip.routeCode
      >>= fromMaybeM (InternalError "Vehicle Route mapping not found")

  routeInfo <-
    QRM.findByRouteCode trip.routeCode
      >>= fromMaybeM (InternalError "Route not found")

  let createStopInfo nameCodeObj point =
        StopInfo
          { name = nameCodeObj.stopName,
            code = nameCodeObj.stopCode,
            lat = Just point.lat,
            long = Just point.lon
          }

  nameCodeObjStart <-
    QRTS.findByLocation routeInfo.startPoint
      >>= fromMaybeM (InternalError "No startPoint found")

  nameCodeObjEnd <-
    QRTS.findByLocation routeInfo.endPoint
      >>= fromMaybeM (InternalError "No endPoint found")

  let source = createStopInfo nameCodeObjStart routeInfo.startPoint
  let destination = createStopInfo nameCodeObjEnd routeInfo.endPoint

  let routeData =
        RouteInfo
          { code = routeInfo.code,
            shortName = routeInfo.shortName,
            longName = routeInfo.longName,
            startPoint = routeInfo.startPoint,
            endPoint = routeInfo.endPoint
          }

  if extra
    then
      return $
        Right
          TripTransactionDetailsExtra
            { tripTransactionId = trip.id.getId,
              vehicleNum = trip.vehicleNumber,
              vehicleType = mapping.vehicleServiceTierType,
              source = source,
              destination = destination,
              status = trip.status,
              routeInfo = routeData
            }
    else
      return $
        Left
          TripTransactionDetails
            { tripTransactionId = trip.id.getId,
              vehicleNum = trip.vehicleNumber,
              vehicleType = mapping.vehicleServiceTierType,
              source = source,
              destination = destination
            }

getUiWmbTripActive ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.WMB.ActiveTripTransaction
  )
getUiWmbTripActive (person, _, _) = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver id not found") person
  let findTrips status = QTTE.findAllTripTransactionByDriverIdStatus driverId (Just 1) (Just 0) (Just status)

  tripT <-
    findTrips IN_PROGRESS >>= \case
      (trip : _) -> pure (Just trip)
      [] ->
        findTrips TRIP_ASSIGNED >>= \case
          (trip : _) -> pure (Just trip)
          [] -> pure Nothing

  case tripT of
    Nothing -> pure $ API.Types.UI.WMB.ActiveTripTransaction Nothing
    Just trip -> do
      result <- getTripTransactionDetails trip True
      pure $ API.Types.UI.WMB.ActiveTripTransaction ((either (const Nothing) Just) result)

findClosestStop :: LatLong -> [(Text, LatLong)] -> Maybe Text
findClosestStop loc stops =
  if null stops
    then Nothing
    else Just $ fst $ minimumBy (comparing (KU.distanceBetweenInMeters loc . snd)) (fromList stops)

postUiWmbTripStart ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripStartReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postUiWmbTripStart (person, _, city) tripTransactionId obj = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  transaction <- QTT.findByTransactionId (Kernel.Types.Id.Id tripTransactionId) >>= fromMaybeM (InternalError "no trip transaction found")
  allStops <- QRTSM.findByRouteCode transaction.routeCode
  let stops = map (\stop -> (stop.tripCode, stop.stopPoint)) allStops
  let closestStop = findClosestStop obj.location stops
  case closestStop of
    Just code -> do
      QTT.updateOnStart code (Just obj.location) IN_PROGRESS (Kernel.Types.Id.Id tripTransactionId)
      TN.notifyWmbOnRide driverId city IN_PROGRESS "Ride Started" "Your ride has started" Nothing
      pure Success
    Nothing -> throwError (InvalidRequest "Could not start trip")

postUiWmbTripEnd ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripEndReq ->
    Environment.Flow API.Types.UI.WMB.TripEndResp
  )
postUiWmbTripEnd (person, _, merch_city_id) tripTransactionId obj = do
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  fleetDriverAssociation <- FDA.findByDriverId driverId True >>= fromMaybeM (InternalError "Fleet Association not found")
  transaction <- QTT.findByTransactionId (Kernel.Types.Id.Id tripTransactionId) >>= fromMaybeM (InternalError "no trip transaction found")
  routeInfo <- QRM.findByRouteCode transaction.routeCode >>= fromMaybeM (InternalError "Route not found")
  fleetConfigInfo <- QFC.findByPrimaryKey (Kernel.Types.Id.Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (InternalError "Fleet Config Info not found")
  now <- getCurrentTime

  let distanceLeft = KU.distanceBetweenInMeters obj.location routeInfo.endPoint
      updateAndRespond response = do
        QDI.updateOnRide False driverId
        QTT.updateStatus COMPLETED (Just routeInfo.endPoint) (Kernel.Types.Id.Id tripTransactionId)
        QV.deleteByDriverid driverId
        TN.notifyWmbOnRide driverId merch_city_id COMPLETED "Ride Ended" "Your ride has ended" Nothing
        pure response

      createDriverRequest = do
        requestId <- generateGUID
        let info =
              WmbEndTripData
                { lat = obj.location.lat,
                  lon = obj.location.lon,
                  reason = Nothing,
                  tripTransactionId = Kernel.Types.Id.Id tripTransactionId
                }
        let driver_req =
              ApprovalRequest
                { id = requestId,
                  requestType = WmbEndTrip info,
                  title = "Your ride has ended",
                  body = "You reached your end stop of this route",
                  status = PENDING,
                  reason = Nothing,
                  createdAt = now,
                  updatedAt = now,
                  merchantId = Nothing,
                  merchantOperatingCityId = Just merch_city_id
                }
        QDR.create driver_req
        pure $ TripEndResp {requestId = Just requestId.getId, result = WAITING_FOR_ADMIN_APPROVAL}

  let rideEndEligibility = fleetConfigInfo.allowEndingMidRoute || distanceLeft <= fleetConfigInfo.endRideDistanceThreshold
  if rideEndEligibility
    then
      if fleetConfigInfo.rideEndApproval
        then -- if the accept gets accepted then only delete the vehicle entry
          createDriverRequest
        else updateAndRespond $ TripEndResp {requestId = Nothing, result = SUCCESS}
    else throwError $ InvalidRequest "Ride Not Eligible For Ending"

getUiWmbTripList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Domain.Types.TripTransaction.TripStatus) ->
    Environment.Flow [API.Types.UI.WMB.TripTransactionDetails]
  )
getUiWmbTripList (person, _, _) limit offset status = do
  checkFleetDriverAssociation person >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  driverId <- fromMaybeM (InternalError "Driver id not found") person
  allTripTransactionDriver <- QTTE.findAllTripTransactionByDriverIdStatus driverId limit offset status
  result <-
    mapM
      ( \tx ->
          (getTripTransactionDetails tx False) >>= \case
            Left txDetails -> return (Just txDetails)
            Right _ -> return Nothing
      )
      allTripTransactionDriver
  let extractedResults = catMaybes result
  pure extractedResults

postUiWmbRequestsCancel ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    Environment.Flow APISuccess
  )
postUiWmbRequestsCancel (mbPersonId, _, _) driverRequestId = do
  Redis.whenWithLockRedis (driverRequestLockKey driverRequestId) 60 $ do
    driverRequest <- QDR.findByPrimaryKey (Kernel.Types.Id.Id driverRequestId) >>= fromMaybeM (InvalidRequest "Driver Request Id not found")
    case mbPersonId of
      Just personId -> unless (driverRequest.requestorId == personId) $ throwError NotAnExecutor
      _ -> pure ()
    unless (isNothing driverRequest.status) $ throwError (InvalidRequest "Request already processed")
    QDR.updateStatusWithReason (Just REVOKED) (Just "Cancelled by driver") (Kernel.Types.Id.Id driverRequestId)
  pure Success

postUiWmbTripRequest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    RequestDetails ->
    Environment.Flow DriverReqResp
  )
postUiWmbTripRequest (mbPersonId, merchantId, merchantOperatingCityId) tripTransactionId req = do
  tripTransaction <- QTT.findByPrimaryKey (Kernel.Types.Id.Id tripTransactionId) >>= fromMaybeM (InvalidRequest "Invalid trip transaction id")
  existingReq <- QDR.findByTripReqAndStatus tripTransaction.id req.requestType Nothing
  when (isJust existingReq) $ throwError (InvalidRequest "Duplicate request")
  driverId <- case mbPersonId of
    Nothing -> pure tripTransaction.driverId
    Just pid -> pure pid
  id <- L.generateGUID
  now <- getCurrentTime
  fleetDriverAssoc <- FDV.findByDriverIdAndActive driverId True
  fleetOwnerId <- case fleetDriverAssoc of
    Nothing -> throwError (InvalidRequest "Driver is not part of this fleet")
    Just assoc -> pure $ Kernel.Types.Id.Id assoc.fleetOwnerId
  let driverRequest =
        DriverRequest
          { id = Kernel.Types.Id.Id id,
            tripTransactionId = Kernel.Types.Id.Id tripTransactionId,
            description = req.description,
            reason = Nothing,
            requestType = req.requestType,
            requesteeId = fleetOwnerId,
            requestorId = driverId,
            status = Nothing,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId
          }
  void $ QDR.create driverRequest

  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  let maybeAppId = (HM.lookup FleetAppletID . exotelMap) =<< transporterConfig.exotelAppIdMapping -- currently only for END_RIDE
  case transporterConfig.fleetAlertThreshold of
    Just threshold -> do
      let triggerFleetAlertTs = secondsToNominalDiffTime threshold
      createJobIn @_ @'FleetAlert (Just merchantId) (Just merchantOperatingCityId) triggerFleetAlertTs $
        FleetAlertJobData
          { fleetOwnerId = fleetOwnerId,
            entityId = driverRequest.id,
            appletId = maybeAppId
          }
    _ -> pure ()
  pure $ DriverReqResp {requestId = id}

driverRequestLockKey :: Text -> Text
driverRequestLockKey reqId = "Driver:Request:Id-" <> reqId
