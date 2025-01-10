{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( postWmbTripRequest,
    postWmbRequestsCancel,
    driverRequestLockKey,
    postWmbAvailableRoutes,
    postWmbQrStart,
    getWmbTripActive,
    postWmbTripStart,
    postWmbTripEnd,
    getWmbTripList,
    postFleetConsent,
    getFleetConfig,
    getWmbRouteDetails,
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
import Domain.Types.FleetConfig
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
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
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
import SharedLogic.WMB
import qualified SharedLogic.WMB as WMB
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.ApprovalRequest as QDR
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetConfig as QFC
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
import qualified Tools.Call as Call
import Tools.Error
import qualified Tools.Notifications as TN

availableRoutes :: (Text, ServiceTierType) -> Text -> Flow AvailableRoute
availableRoutes (routeCode, vehicleServiceTierType) vhclNo = do
  route <- QR.findByRouteCode routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  (sourceStopInfo, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route routeCode
  pure $
    AvailableRoute
      { routeInfo = buildRouteInfo route,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        roundRouteCode = route.roundRouteCode,
        vehicleDetails =
          VehicleDetails
            { number = vhclNo,
              _type = vehicleServiceTierType
            }
      }

postWmbAvailableRoutes ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.AvailableRouteReq ->
    Flow [API.Types.UI.WMB.AvailableRoute]
  )
postWmbAvailableRoutes (_, _, _) req = do
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
      =<< VRM.findAllRouteMappings vehicleNumberHash
  pure result

postWmbQrStart ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripQrStartReq ->
    Flow API.Types.UI.WMB.TripTransactionDetails
  )
postWmbQrStart (mbDriverId, merchantId, merchantOperatingCityId) req = do
  driverId <- fromMaybeM (InvalidRequest "Driver ID not found") mbDriverId
  vehicleNumberHash <-
    case (A.fromJSON $ A.String req.vehicleNumberHash :: A.Result DbHash) of
      A.Success vehicleNumberHash -> pure vehicleNumberHash
      A.Error err -> throwError $ InternalError (T.pack err)
  vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash req.routeCode >>= fromMaybeM (InvalidRequest "Vehicle Route mapping not found")
  route <- QR.findByRouteCode req.routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  fleetConfig <- QFC.findByPrimaryKey vehicleRouteMapping.fleetOwnerId >>= fromMaybeM (InvalidRequest "Fleet Config Info not found")
  vehicleNumber <- decrypt vehicleRouteMapping.vehicleNumber
  curVehicleDetails <- QV.findByRegistrationNo vehicleNumber
  case curVehicleDetails of
    Just x | x.driverId /= driverId -> throwError (InvalidRequest "Already Linked")
    _ -> pure ()
  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
  tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus driverId
  case tripTransactions of
    [] -> pure ()
    _ -> throwError (InvalidRequest "Could not link")
  FDV.createFleetDriverAssociationIfNotExists driverId vehicleRouteMapping.fleetOwnerId True
  tripTransaction <- WMB.assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber destinationStopInfo req.location
  pure $
    TripTransactionDetails
      { tripTransactionId = tripTransaction.id,
        vehicleNumber = vehicleNumber,
        vehicleType = vehicleRouteMapping.vehicleServiceTierType,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        status = IN_PROGRESS,
        routeInfo = buildRouteInfo route
      }

getTripTransactionDetails ::
  Domain.Types.TripTransaction.TripTransaction ->
  Flow TripTransactionDetails
getTripTransactionDetails tripTransaction = do
  route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  (sourceStopInfo, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route tripTransaction.routeCode
  return $
    TripTransactionDetails
      { tripTransactionId = tripTransaction.id,
        vehicleNumber = tripTransaction.vehicleNumber,
        vehicleType = tripTransaction.vehicleServiceTierType,
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
  WMB.findNextEligibleTripTransactionByDriverIdStatus driverId IN_PROGRESS
    |<|>| WMB.findNextEligibleTripTransactionByDriverIdStatus driverId TRIP_ASSIGNED
      >>= \case
        Nothing -> pure $ API.Types.UI.WMB.ActiveTripTransaction Nothing
        Just tripTransaction -> do
          tripTransactionDetails <- getTripTransactionDetails tripTransaction
          pure $ API.Types.UI.WMB.ActiveTripTransaction (Just tripTransactionDetails)

postWmbTripStart ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id TripTransaction ->
    API.Types.UI.WMB.TripStartReq ->
    Flow APISuccess
  )
postWmbTripStart (_, _, _) tripTransactionId req = do
  tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (InternalError "no trip transaction found")
  WMB.checkFleetDriverAssociation tripTransaction.driverId tripTransaction.fleetOwnerId >>= \isAssociated -> unless isAssociated (throwError $ InternalError "Fleet-driver association not found")
  allStops <- QRTSM.findByRouteCode tripTransaction.routeCode
  let stops = map (\stop -> WMB.StopData stop.tripCode stop.routeCode stop.stopCode stop.stopPoint) allStops
  let closestStopTripCode = case WMB.findClosestStop req.location stops of
        Just stop -> (Just stop.tripCode)
        Nothing -> Nothing
  case closestStopTripCode of
    Just tripCode -> do
      tripEndStop <- (listToMaybe $ sortBy (EHS.comparing (.stopSequenceNum)) $ filter (\stop -> stop.tripCode == tripCode) allStops) & fromMaybeM (InternalError "End Stop Not Found.")
      let busTripInfo = WMB.buildBusTripInfo tripTransaction.vehicleNumber tripTransaction.routeCode tripEndStop.stopPoint
      void $ LF.rideStart (cast tripTransaction.id) req.location.lat req.location.lon tripTransaction.merchantId tripTransaction.driverId (Just busTripInfo)
      QTT.updateOnStart (Just tripCode) (Just req.location) IN_PROGRESS tripTransactionId
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
postWmbTripEnd (person, _, _) tripTransactionId req = do
  driverId <- fromMaybeM (InternalError "Driver ID not found") person
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (InternalError "Fleet Association not found")
  tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (InternalError "no trip transaction found")
  when (tripTransaction.status == COMPLETED) $ throwError (InvalidRequest "Trip already ended.")
  awaitingRequests <- QDR.findByStatus driverId AWAITING_APPROVAL
  when (any (\ar -> case ar.requestData of EndRide endRideData -> endRideData.tripTransactionId == tripTransactionId; _ -> False) awaitingRequests) $ throwError (InvalidRequest "EndRide request already present.")
  route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (InternalError "Route not found")
  fleetConfig <- QFC.findByPrimaryKey (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (InternalError "Fleet Config Info not found")
  let distanceLeft = KU.distanceBetweenInMeters req.location route.endPoint
      rideEndEligibility = fleetConfig.allowEndingMidRoute || distanceLeft <= fleetConfig.endRideDistanceThreshold
  unless rideEndEligibility $
    throwError $ InvalidRequest "Ride Not Eligible For Ending"
  if fleetConfig.rideEndApproval
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
      void $ endTripTransaction fleetConfig tripTransaction req.location
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
  let sortType = if maybe False WMB.isNonTerminalTripStatus status then QTTE.SortAsc else QTTE.SortDesc
  allTripTransactionDriver <- QTTE.findAllTripTransactionByDriverIdStatus driverId limit offset status sortType
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

postFleetConsent ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow APISuccess
  )
postFleetConsent (mbDriverId, _, merchantOperatingCityId) = do
  driverId <- fromMaybeM (InternalError "Driver id not found") mbDriverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  fleetDriverAssociation <- FDV.findByDriverId driverId False >>= fromMaybeM (InternalError "Inactive Fleet Driver Association Not Found")
  fleetOwner <- QPerson.findById (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (InvalidRequest "Fleet Owner not found")
  FDV.upsert fleetDriverAssociation {isActive = True}
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId "FLEET_CONSENT" Nothing Nothing driver.language
  whenJust mbMerchantPN $ \merchantPN -> do
    let title = T.replace "{#fleetOwnerName#}" fleetOwner.firstName merchantPN.title
    let body = T.replace "{#fleetOwnerName#}" fleetOwner.firstName merchantPN.body
    TN.notifyDriver merchantOperatingCityId merchantPN.fcmNotificationType title body driver driver.deviceToken
  pure Success

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

buildRouteInfo :: Route -> RouteInfo
buildRouteInfo Route {..} = RouteInfo {..}

getFleetConfig ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow FleetConfig
  )
getFleetConfig (mbDriverId, _, _) = do
  driverId <- fromMaybeM (InternalError "Driver id not found") mbDriverId
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (InvalidRequest "No Active Fleet Associated")
  fleetConfig <- QFC.findByPrimaryKey (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (InvalidRequest "Fleet Config Info not found")
  pure $ fleetConfig

getWmbRouteDetails ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Text ->
    Flow API.Types.UI.WMB.RouteDetails
  )
getWmbRouteDetails (_, _, _) routeCode = do
  route <- QR.findByRouteCode routeCode >>= fromMaybeM (InvalidRequest "Route not found")
  let waypoints = case route.polyline of
        Just polyline -> Just (KEPP.decode polyline)
        Nothing -> Nothing
  now <- getCurrentTime
  let day = dayOfWeek (utctDay now)
  stops <- QRTSM.findAllByRouteCodeForStops routeCode 1 day
  let stopInfos = map (\stop -> StopInfo (stop.stopCode) (stop.stopName) (stop.stopPoint)) (sortBy (EHS.comparing stopSequenceNum) stops)
  pure $
    RouteDetails
      { code = routeCode,
        shortName = route.shortName,
        longName = route.longName,
        startPoint = route.startPoint,
        endPoint = route.endPoint,
        stops = stopInfos,
        waypoints = waypoints,
        timeBounds = (Just route.timeBounds)
      }
