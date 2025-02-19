{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.WMB
  ( postWmbTripRequest,
    getWmbRequestsStatus,
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

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
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
import Domain.Types.EmptyDynamicParam
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
import qualified Domain.Types.VehicleCategory as DVehCategory
import qualified Domain.Types.VehicleVariant as DVehVariant
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
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
import qualified Storage.Queries.TransporterConfig as QTC
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.TripTransactionExtra as QTTE
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRouteMapping as VRM
import qualified Tools.Call as Call
import Tools.Error
import qualified Tools.Notifications as TN

availableRoutes :: (Text, ServiceTierType) -> Text -> Flow AvailableRoute
availableRoutes (routeCode, vehicleServiceTierType) vhclNo = do
  route <- QR.findByRouteCode routeCode >>= fromMaybeM (RouteNotFound routeCode)
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
          vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNumber >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
          availableRoutes (mapping.routeCode, maybe BUS_NON_AC DVehVariant.castVariantToServiceTier vehicleRC.vehicleVariant) vehicleNumber
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
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  vehicleNumberHash <-
    case (A.fromJSON $ A.String req.vehicleNumberHash :: A.Result DbHash) of
      A.Success vehicleNumberHash -> pure vehicleNumberHash
      A.Error err -> throwError $ InternalError (T.pack err)

  vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash req.routeCode >>= fromMaybeM (VehicleRouteMappingNotFound req.vehicleNumberHash req.routeCode)
  when (vehicleRouteMapping.blocked) $ throwError (VehicleRouteMappingBlocked)
  route <- QR.findByRouteCode req.routeCode >>= fromMaybeM (RouteNotFound req.routeCode)
  fleetConfig <- QFC.findByPrimaryKey vehicleRouteMapping.fleetOwnerId >>= fromMaybeM (FleetConfigNotFound vehicleRouteMapping.fleetOwnerId.getId)

  vehicleNumber <- decrypt vehicleRouteMapping.vehicleNumber
  curVehicleDetails <- QV.findByRegistrationNo vehicleNumber
  case curVehicleDetails of
    Just x | x.driverId /= driverId -> throwError (VehicleLinkedToAnotherDriver vehicleNumber)
    _ -> pure ()
  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
  tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus driverId
  case tripTransactions of
    [] -> pure ()
    _ -> throwError (InvalidTripStatus "IN_PROGRESS")
  FDV.createFleetDriverAssociationIfNotExists driverId vehicleRouteMapping.fleetOwnerId DVehCategory.BUS True
  tripTransaction <- WMB.assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber destinationStopInfo req.location
  pure $
    TripTransactionDetails
      { tripTransactionId = tripTransaction.id,
        endRideApprovalRequestId = tripTransaction.endRideApprovalRequestId,
        vehicleNumber = vehicleNumber,
        vehicleType = tripTransaction.vehicleServiceTierType,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        status = IN_PROGRESS,
        routeInfo = buildRouteInfo route
      }

getTripTransactionDetails ::
  Domain.Types.TripTransaction.TripTransaction ->
  Flow TripTransactionDetails
getTripTransactionDetails tripTransaction = do
  route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
  (sourceStopInfo, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route tripTransaction.routeCode
  return $
    TripTransactionDetails
      { tripTransactionId = tripTransaction.id,
        endRideApprovalRequestId = tripTransaction.endRideApprovalRequestId,
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
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  WMB.findNextActiveTripTransaction driverId
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
postWmbTripStart (mbDriverId, _, _) tripTransactionId req = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  tripTransaction <-
    WMB.findNextActiveTripTransaction driverId
      >>= \case
        Nothing -> throwError $ TripTransactionNotFound tripTransactionId.getId
        Just tripTransaction -> do
          unless (tripTransactionId == tripTransaction.id && tripTransaction.status == TRIP_ASSIGNED) $ throwError AlreadyOnActiveTrip
          return tripTransaction
  WMB.checkFleetDriverAssociation tripTransaction.driverId tripTransaction.fleetOwnerId >>= \isAssociated -> unless isAssociated (throwError $ DriverNotLinkedToFleet tripTransaction.driverId.getId)
  closestStop <- WMB.findClosestStop tripTransaction.routeCode req.location >>= fromMaybeM (StopNotFound)
  route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
  (_, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route tripTransaction.routeCode
  void $ WMB.startTripTransaction tripTransaction route closestStop (LatLong req.location.lat req.location.lon) destinationStopInfo.point True
  pure Success

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
  driverId <- fromMaybeM (DriverNotFoundWithId) person
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (NoActiveFleetAssociated driverId.getId)
  tripTransaction <- QTT.findByTransactionId tripTransactionId >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
  route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
  fleetConfig <- QFC.findByPrimaryKey (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetDriverAssociation.fleetOwnerId)
  let distanceLeft = KU.distanceBetweenInMeters req.location route.endPoint
      rideEndEligibility = fleetConfig.allowEndingMidRoute || distanceLeft <= fleetConfig.endRideDistanceThreshold
  unless rideEndEligibility $
    throwError $ RideNotEligibleForEnding
  if fleetConfig.rideEndApproval && not (distanceLeft <= fleetConfig.endRideDistanceThreshold)
    then do
      whenJust tripTransaction.endRideApprovalRequestId $ \endRideApprovalRequestId -> do
        approvalRequest <- QDR.findByPrimaryKey endRideApprovalRequestId
        when ((approvalRequest <&> (.status)) == Just AWAITING_APPROVAL) $ throwError EndRideRequestAlreadyPresent
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      driverMobileNumber <- mapM decrypt driver.mobileNumber
      let requestData =
            EndRide
              EndRideData
                { lat = req.location.lat,
                  lon = req.location.lon,
                  tripTransactionId = tripTransaction.id.getId,
                  vehicleRegistrationNumber = tripTransaction.vehicleNumber,
                  driverName = driver.firstName <> " " <> (fromMaybe "" driver.lastName),
                  driverMobileNumber = driverMobileNumber,
                  tripCode = tripTransaction.tripCode
                }
          requestTitle = "Your trip has ended!"
          requestBody = "You reached your end stop of this route."
      Redis.whenWithLockRedisAndReturnValue
        (WMB.tripTransactionKey driverId COMPLETED)
        60
        ( do
            unless (tripTransaction.status == IN_PROGRESS) $ throwError (InvalidTripStatus $ show tripTransaction.status)
            createDriverRequest driverId fleetDriverAssociation.fleetOwnerId requestTitle requestBody requestData tripTransaction
        )
        >>= \case
          Right driverReq -> return $ TripEndResp {requestId = Just driverReq.id.getId, result = WAITING_FOR_ADMIN_APPROVAL}
          Left _ -> throwError (InternalError "Process for Trip End is Already Ongoing, Please try again!")
    else do
      void $ endTripTransaction fleetConfig tripTransaction req.location DriverDirect
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
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
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
    approvalRequest <- QDR.findByPrimaryKey approvalRequestId >>= fromMaybeM (ApprovalRequestIdNotFound approvalRequestId.getId)
    case mbPersonId of
      Just personId -> unless (approvalRequest.requestorId == personId) $ throwError NotAnExecutor
      _ -> pure ()
    unless (approvalRequest.status == AWAITING_APPROVAL) $ throwError (RequestAlreadyProcessed approvalRequest.id.getId)
    QDR.updateStatusWithReason REVOKED (Just "Cancelled by driver") approvalRequestId
  pure Success

getWmbRequestsStatus ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id ApprovalRequest ->
    Flow ApprovalRequestResp
  )
getWmbRequestsStatus (mbPersonId, _, _) approvalRequestId = do
  approvalRequest <- QDR.findByPrimaryKey approvalRequestId >>= fromMaybeM (ApprovalRequestIdNotFound approvalRequestId.getId)
  whenJust mbPersonId $ \personId -> unless (approvalRequest.requestorId == personId) $ throwError NotAnExecutor
  pure $ ApprovalRequestResp {status = approvalRequest.status}

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
  tripTransaction <- QTT.findByPrimaryKey tripTransactionId >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
  fleetDriverAssociation <- FDV.findByDriverId tripTransaction.driverId True >>= fromMaybeM (NoActiveFleetAssociated tripTransaction.driverId.getId)
  whenJust tripTransaction.endRideApprovalRequestId $ \endRideApprovalRequestId -> do
    approvalRequest <- QDR.findByPrimaryKey endRideApprovalRequestId
    when ((approvalRequest <&> (.status)) == Just AWAITING_APPROVAL) $ throwError EndRideRequestAlreadyPresent
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

postFleetConsent ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow APISuccess
  )
postFleetConsent (mbDriverId, _, merchantOperatingCityId) = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  fleetDriverAssociation <- FDV.findByDriverId driverId False >>= fromMaybeM (InactiveFleetDriverAssociationNotFound driverId.getId)
  onboardingVehicleCategory <- fleetDriverAssociation.onboardingVehicleCategory & fromMaybeM DriverOnboardingVehicleCategoryNotFound
  fleetOwner <- QPerson.findById (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (FleetOwnerNotFound fleetDriverAssociation.fleetOwnerId)
  FDV.updateByPrimaryKey (fleetDriverAssociation {isActive = True})
  QDriverInfoInternal.updateOnboardingVehicleCategory (Just onboardingVehicleCategory) driver.id
  QDI.updateEnabledVerifiedState driverId True (Just True)
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId "FLEET_CONSENT" Nothing Nothing driver.language Nothing
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
  QTT.updateEndRideApprovalRequestId (Just id) tripTransaction.id
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
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (NoActiveFleetAssociated driverId.getId)
  fleetConfig <- QFC.findByPrimaryKey (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetDriverAssociation.fleetOwnerId)
  pure $ fleetConfig

getWmbRouteDetails ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Text ->
    Flow Common.RouteDetails
  )
getWmbRouteDetails (_, _, _) routeCode = do
  getRouteDetails routeCode
