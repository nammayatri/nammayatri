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
    getWmbFleetBadges,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
import API.Types.UI.WMB
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import Domain.Types.Alert
import Domain.Types.AlertRequest
import Domain.Types.Common
import Domain.Types.Extra.TransporterConfig
import qualified Domain.Types.FleetBadgeType as DFBT
import Domain.Types.FleetConfig
import Domain.Types.FleetDriverAssociation
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.Route
import Domain.Types.TripTransaction
import qualified Domain.Types.VehicleCategory as DVehCategory
import qualified Domain.Types.VehicleVariant as DVehVariant
import Environment
import Kernel.External.Encryption
import Kernel.External.Maps.Types hiding (fromList)
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import SharedLogic.WMB
import qualified SharedLogic.WMB as WMB
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.Cac.TransporterConfig as SCT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.AlertRequest as QAR
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import qualified Storage.Queries.FleetBadge as QFB
import qualified Storage.Queries.FleetBadgeAssociation as QFBA
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Route as QR
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.TripTransactionExtra as QTTE
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRouteMapping as VRM
import Tools.Error
import qualified Tools.Notifications as TN
import Utils.Common.Cac.KeyNameConstants

getWmbFleetBadges ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Maybe Text ->
    Maybe DFBT.FleetBadgeType ->
    Int ->
    Int ->
    Flow [API.Types.UI.WMB.AvailableBadge]
  )
getWmbFleetBadges (mbDriverId, _, merchantOperatingCityId) mbSearchString mbBadgeType limit offset = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (NoActiveFleetAssociated driverId.getId)
  fleetBadgesByOwner <- QFB.findAllMatchingBadges mbSearchString (Just $ toInteger limit) (Just $ toInteger offset) merchantOperatingCityId fleetDriverAssociation.fleetOwnerId mbBadgeType
  mapM
    ( \badge -> do
        driverBadgeAssociation <- QFBA.findActiveFleetBadgeAssociationById badge.id badge.badgeType
        pure $ API.Types.UI.WMB.AvailableBadge badge.badgeName badge.badgeType (isJust driverBadgeAssociation)
    )
    fleetBadgesByOwner

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
postWmbAvailableRoutes (mbDriverId, _, _) req = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  vehicleNumberHash <-
    case req.vehicleNumber of
      Nothing -> do
        vehicle <- QV.findById driverId >>= fromMaybeM (VehicleNotFound $ "driverId:-" <> driverId.getId)
        getDbHash vehicle.registrationNo
      -- This Flow with QR Scan with Vehicle Number Hash is Deprecated, have kept it for sometime then can be removed later.
      Just vehicleNumber -> do
        -- -- HACK TO MAKE EXISTING DbHash work, as it internally does encodeHex (in ToJSON) defore doing db query.
        case (A.fromJSON $ A.String vehicleNumber :: A.Result DbHash) of
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
    case req.vehicleNumberHash of
      Nothing -> do
        vehicle <- QV.findById driverId >>= fromMaybeM (VehicleNotFound $ "driverId:-" <> driverId.getId)
        getDbHash vehicle.registrationNo
      -- This Flow with QR Scan with Vehicle Number Hash is Deprecated, have kept it for sometime then can be removed later.
      Just vehicleNumber -> do
        -- -- HACK TO MAKE EXISTING DbHash work, as it internally does encodeHex (in ToJSON) defore doing db query.
        case (A.fromJSON $ A.String vehicleNumber :: A.Result DbHash) of
          A.Success vehicleNumberHash -> pure vehicleNumberHash
          A.Error err -> throwError $ InternalError (T.pack err)
  vehicleRouteMapping <- VRM.findOneMapping vehicleNumberHash req.routeCode >>= fromMaybeM (VehicleRouteMappingNotFound (show vehicleNumberHash) req.routeCode)
  when (vehicleRouteMapping.blocked) $ throwError (VehicleRouteMappingBlocked)
  route <- QR.findByRouteCode req.routeCode >>= fromMaybeM (RouteNotFound req.routeCode)
  fleetConfig <- QFC.findByPrimaryKey vehicleRouteMapping.fleetOwnerId >>= fromMaybeM (FleetConfigNotFound vehicleRouteMapping.fleetOwnerId.getId)

  vehicleNumber <- decrypt vehicleRouteMapping.vehicleNumber
  curVehicleDetails <- QV.findByRegistrationNo vehicleNumber
  case curVehicleDetails of
    Just x | x.driverId /= driverId -> throwError (VehicleLinkedToAnotherDriver vehicleNumber)
    _ -> pure ()
  (sourceStopInfo, destinationStopInfo) <- getSourceAndDestinationStopInfo route route.code
  tripTransactions <- QTTE.findAllTripTransactionByDriverIdActiveStatus vehicleRouteMapping.fleetOwnerId (Just 1) driverId
  case tripTransactions of
    [] -> pure ()
    _ -> throwError (InvalidTripStatus "IN_PROGRESS")
  mbDriverBadge <-
    case req.driverBadgeName of
      Just driverBadgeName -> do
        driverBadge <- WMB.validateBadgeAssignment driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId driverBadgeName DFBT.DRIVER
        WMB.linkFleetBadge driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId driverBadge DFBT.DRIVER
        return $ Just driverBadge
      Nothing -> pure Nothing
  mbConductorBadge <-
    case req.conductorBadgeName of
      Just conductorBadgeName -> do
        conductorBadge <- WMB.validateBadgeAssignment driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId conductorBadgeName DFBT.CONDUCTOR
        WMB.linkFleetBadge driverId merchantId merchantOperatingCityId fleetConfig.fleetOwnerId.getId conductorBadge DFBT.CONDUCTOR
        return $ Just conductorBadge
      Nothing -> pure Nothing
  FDV.createFleetDriverAssociationIfNotExists driverId vehicleRouteMapping.fleetOwnerId Nothing DVehCategory.BUS True Nothing
  tripTransaction <-
    if fleetConfig.directlyStartFirstTripAssignment
      then WMB.assignAndStartTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber sourceStopInfo destinationStopInfo req.location DriverDirect (mbDriverBadge <&> (.id)) (mbDriverBadge <&> (.badgeName)) (mbConductorBadge <&> (.id)) (mbConductorBadge <&> (.badgeName))
      else WMB.assignTripTransaction fleetConfig merchantId merchantOperatingCityId driverId route vehicleRouteMapping vehicleNumber req.location sourceStopInfo destinationStopInfo (mbDriverBadge <&> (.id)) (mbDriverBadge <&> (.badgeName)) (mbConductorBadge <&> (.id)) (mbConductorBadge <&> (.badgeName))
  pure $
    TripTransactionDetails
      { tripTransactionId = tripTransaction.id,
        endRideApprovalRequestId = tripTransaction.endRideApprovalRequestId,
        driverName = tripTransaction.driverName,
        conductorName = tripTransaction.conductorName,
        vehicleNumber = vehicleNumber,
        vehicleType = tripTransaction.vehicleServiceTierType,
        source = sourceStopInfo,
        destination = destinationStopInfo,
        status = tripTransaction.status,
        routeInfo = buildRouteInfo route,
        dutyType = tripTransaction.dutyType,
        vipName = tripTransaction.vipName,
        scheduledTripTime = tripTransaction.scheduledTripTime,
        tripType = tripTransaction.tripType
      }

getTripTransactionDetails ::
  Domain.Types.TripTransaction.TripTransaction ->
  Flow TripTransactionDetails
getTripTransactionDetails tripTransaction = do
  case tripTransaction.tripType of
    Just PILOT -> do
      let source = fromMaybe (LatLong 0.0 0.0) tripTransaction.pilotSource
          destination = fromMaybe (LatLong 0.0 0.0) tripTransaction.pilotDestination
          startAddress = fromMaybe "" tripTransaction.startAddress
          endAddress = fromMaybe "" tripTransaction.endAddress
      return $
        TripTransactionDetails
          { tripTransactionId = tripTransaction.id,
            endRideApprovalRequestId = tripTransaction.endRideApprovalRequestId,
            driverName = tripTransaction.driverName,
            conductorName = tripTransaction.conductorName,
            vehicleNumber = tripTransaction.vehicleNumber,
            vehicleType = tripTransaction.vehicleServiceTierType,
            source = StopInfo startAddress "NONE" source,
            destination = StopInfo endAddress "NONE" destination,
            status = tripTransaction.status,
            routeInfo = dummyRouteInfo,
            dutyType = tripTransaction.dutyType,
            vipName = tripTransaction.vipName,
            scheduledTripTime = tripTransaction.scheduledTripTime,
            tripType = tripTransaction.tripType
          }
    _ -> do
      route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
      (sourceStopInfo, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route tripTransaction.routeCode
      return $
        TripTransactionDetails
          { tripTransactionId = tripTransaction.id,
            endRideApprovalRequestId = tripTransaction.endRideApprovalRequestId,
            driverName = tripTransaction.driverName,
            conductorName = tripTransaction.conductorName,
            vehicleNumber = tripTransaction.vehicleNumber,
            vehicleType = tripTransaction.vehicleServiceTierType,
            source = sourceStopInfo,
            destination = destinationStopInfo,
            status = tripTransaction.status,
            routeInfo = buildRouteInfo route,
            dutyType = tripTransaction.dutyType,
            vipName = tripTransaction.vipName,
            scheduledTripTime = tripTransaction.scheduledTripTime,
            tripType = tripTransaction.tripType
          }

dummyRouteInfo :: API.Types.UI.WMB.RouteInfo
dummyRouteInfo = API.Types.UI.WMB.RouteInfo {code = "", shortName = "", longName = "", startPoint = LatLong 0.0 0.0, endPoint = LatLong 0.0 0.0}

getWmbTripActive ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow API.Types.UI.WMB.ActiveTripTransaction
  )
getWmbTripActive (mbDriverId, _, _) = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (NoActiveFleetAssociated driverId.getId)
  WMB.findNextActiveTripTransaction fleetDriverAssociation.fleetOwnerId driverId
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
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (NoActiveFleetAssociated driverId.getId)
  tripTransaction <-
    WMB.findNextActiveTripTransaction fleetDriverAssociation.fleetOwnerId driverId
      >>= \case
        Nothing -> throwError $ TripTransactionNotFound tripTransactionId.getId
        Just tripTransaction -> do
          unless (tripTransactionId == tripTransaction.id && tripTransaction.status == TRIP_ASSIGNED) $ throwError AlreadyOnActiveTrip
          return tripTransaction

  -- Handle based on trip type
  case tripTransaction.tripType of
    Just PILOT -> do
      let source = fromMaybe (LatLong req.location.lat req.location.lon) tripTransaction.pilotSource
      let destination = fromMaybe (LatLong req.location.lat req.location.lon) tripTransaction.pilotDestination
      let sourceStopInfo = StopInfo "" "" source
      void $ WMB.startTripTransaction tripTransaction Nothing Nothing sourceStopInfo (LatLong req.location.lat req.location.lon) destination True DriverDirect
    _ -> do
      closestStop <- WMB.findClosestStop tripTransaction.routeCode req.location >>= fromMaybeM (StopNotFound)
      route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
      (sourceStopInfo, destinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route tripTransaction.routeCode
      void $ WMB.startTripTransaction tripTransaction (Just route) (Just closestStop) sourceStopInfo (LatLong req.location.lat req.location.lon) destinationStopInfo.point True DriverDirect
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
  fleetConfig <- QFC.findByPrimaryKey (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetDriverAssociation.fleetOwnerId)

  -- Calculate distance and eligibility based on trip type
  (distanceLeft, rideEndEligibility) <- case tripTransaction.tripType of
    Just PILOT -> do
      let dest = fromMaybe (LatLong req.location.lat req.location.lon) tripTransaction.pilotDestination
          distance = KU.distanceBetweenInMeters req.location dest
      pure (distance, True) -- Always allow ending for PILOT
    _ -> do
      route <- QR.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
      let distanceLeft = KU.distanceBetweenInMeters req.location route.endPoint
      pure (distanceLeft, fleetConfig.allowEndingMidRoute || distanceLeft <= fleetConfig.endRideDistanceThreshold)

  unless rideEndEligibility $
    throwError $ RideNotEligibleForEnding

  -- Skip ride end approval for PILOT trips
  if tripTransaction.tripType /= Just PILOT && fleetConfig.rideEndApproval && not (distanceLeft <= fleetConfig.endRideDistanceThreshold)
    then do
      whenJust tripTransaction.endRideApprovalRequestId $ \endRideAlertRequestId -> do
        alertRequest <- QAR.findByPrimaryKey endRideAlertRequestId
        when ((alertRequest <&> (.status)) == Just AWAITING_APPROVAL) $ throwError EndRideRequestAlreadyPresent
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
                  tripCode = tripTransaction.tripCode,
                  distance = Just distanceLeft
                }
          requestTitle = "Your trip has ended!"
          requestBody = "You reached your end stop of this route."
      Redis.whenWithLockRedisAndReturnValue
        (WMB.tripTransactionKey driverId COMPLETED)
        60
        ( do
            unless (tripTransaction.status == IN_PROGRESS) $ throwError (InvalidTripStatus $ show tripTransaction.status)
            createAlertRequest driverId fleetDriverAssociation.fleetOwnerId requestTitle requestBody requestData tripTransaction
        )
        >>= \case
          Right alertRequestId -> return $ TripEndResp {requestId = Just alertRequestId.getId, result = WAITING_FOR_ADMIN_APPROVAL}
          Left _ -> throwError (InternalError "Process for Trip End is Already Ongoing, Please try again!")
    else do
      unless (WMB.isNonTerminalTripStatus tripTransaction.status) $ throwError (InvalidTripStatus $ show tripTransaction.status)
      if tripTransaction.status == IN_PROGRESS
        then void $ WMB.endOngoingTripTransaction fleetConfig tripTransaction req.location DriverDirect False
        else WMB.cancelTripTransaction fleetConfig tripTransaction req.location DriverDirect
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
  fleetDriverAssociation <- FDV.findByDriverId driverId True >>= fromMaybeM (NoActiveFleetAssociated driverId.getId)
  let sortType = if maybe False WMB.isNonTerminalTripStatus status then QTTE.SortAsc else QTTE.SortDesc
  allTripTransactionDriver <- QTTE.findAllTripTransactionByDriverIdStatus (Id fleetDriverAssociation.fleetOwnerId) driverId limit offset status sortType
  tripTransactions <- mapM getTripTransactionDetails allTripTransactionDriver
  pure tripTransactions

postWmbRequestsCancel ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id AlertRequest ->
    Flow APISuccess
  )
postWmbRequestsCancel (mbPersonId, _, _) alertRequestId = do
  Redis.whenWithLockRedis (driverRequestLockKey alertRequestId.getId) 60 $ do
    alertRequest <- QAR.findByPrimaryKey alertRequestId >>= fromMaybeM (AlertRequestIdNotFound alertRequestId.getId)
    case mbPersonId of
      Just personId -> unless (alertRequest.requestorId == personId) $ throwError NotAnExecutor
      _ -> pure ()
    unless (alertRequest.status == AWAITING_APPROVAL) $ throwError (RequestAlreadyProcessed alertRequest.id.getId)
    WMB.updateAlertRequestStatus REVOKED (Just "Cancelled by driver") alertRequestId
  pure Success

getWmbRequestsStatus ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id AlertRequest ->
    Flow AlertRequestResp
  )
getWmbRequestsStatus (mbPersonId, _, _) alertRequestId = do
  alertRequest <- QAR.findByPrimaryKey alertRequestId >>= fromMaybeM (AlertRequestIdNotFound alertRequestId.getId)
  whenJust mbPersonId $ \personId -> unless (alertRequest.requestorId == personId) $ throwError NotAnExecutor
  pure $ AlertRequestResp {status = alertRequest.status}

postWmbTripRequest ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Id TripTransaction ->
    RequestDetails ->
    Flow AlertReqResp
  )
postWmbTripRequest (_, merchantId, merchantOperatingCityId) tripTransactionId req = do
  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  tripTransaction <- QTT.findByPrimaryKey tripTransactionId >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
  fleetDriverAssociation <- FDV.findByDriverId tripTransaction.driverId True >>= fromMaybeM (NoActiveFleetAssociated tripTransaction.driverId.getId)
  whenJust tripTransaction.endRideApprovalRequestId $ \endRideAlertRequestId -> do
    alertRequest <- QAR.findByPrimaryKey endRideAlertRequestId
    when ((alertRequest <&> (.status)) == Just AWAITING_APPROVAL) $ throwError EndRideRequestAlreadyPresent
  alertRequestId <- createAlertRequest tripTransaction.driverId fleetDriverAssociation.fleetOwnerId req.title req.body req.requestData tripTransaction
  let maybeAppId = (HM.lookup FleetAppletID . exotelMap) =<< transporterConfig.exotelAppIdMapping -- currently only for END_RIDE
  whenJust transporterConfig.fleetAlertThreshold $ \threshold -> do
    let triggerFleetAlertTs = secondsToNominalDiffTime threshold
    createJobIn @_ @'FleetAlert (Just merchantId) (Just merchantOperatingCityId) triggerFleetAlertTs $
      FleetAlertJobData
        { fleetOwnerId = Id fleetDriverAssociation.fleetOwnerId,
          entityId = alertRequestId,
          appletId = maybeAppId
        }
  pure $ AlertReqResp {requestId = alertRequestId.getId}

postFleetConsent ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow APISuccess
  )
postFleetConsent (mbDriverId, merchantId, merchantOperatingCityId) = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  fleetDriverAssociation <- FDV.findByDriverId driverId False >>= fromMaybeM (InactiveFleetDriverAssociationNotFound driverId.getId)

  when (isJust fleetDriverAssociation.requestReason) $ throwError $ InvalidRequest "Cannot consent to driver-initiated request. Fleet owner must approve first"
  onboardingVehicleCategory <- fleetDriverAssociation.onboardingVehicleCategory & fromMaybeM DriverOnboardingVehicleCategoryNotFound
  fleetOwner <- QPerson.findById (Id fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (FleetOwnerNotFound fleetDriverAssociation.fleetOwnerId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  transporterConfig <- SCT.findByMerchantOpCityId merchantOperatingCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)

  SA.endDriverAssociationsIfAllowed merchant merchantOperatingCityId transporterConfig driver

  FDV.updateByPrimaryKey (fleetDriverAssociation {isActive = True})
  when (transporterConfig.deleteDriverBankAccountWhenLinkToFleet == Just True) $ QDBA.deleteById driverId
  Analytics.handleDriverAnalyticsAndFlowStatus
    transporterConfig
    fleetDriverAssociation.driverId
    Nothing
    ( \driverInfo -> do
        Analytics.incrementFleetOwnerAnalyticsActiveDriverCount (Just fleetDriverAssociation.fleetOwnerId) driver.id
        mOperator <- QFOA.findByFleetOwnerId fleetDriverAssociation.fleetOwnerId True
        when (isNothing mOperator) $ logTagError "AnalyticsAddDriver" "Operator not found for fleet owner"
        whenJust mOperator $ \operator -> do
          when driverInfo.enabled $ Analytics.incrementOperatorAnalyticsDriverEnabled transporterConfig operator.operatorId
          Analytics.incrementOperatorAnalyticsActiveDriver transporterConfig operator.operatorId
    )
    ( \driverInfo -> do
        DDriverMode.incrementFleetOperatorStatusKeyForDriver FLEET_OWNER fleetDriverAssociation.fleetOwnerId driverInfo.driverFlowStatus
    )
  QDriverInfoInternal.updateOnboardingVehicleCategory (Just onboardingVehicleCategory) driver.id

  whenJust fleetDriverAssociation.onboardedOperatorId $ \referredOperatorId -> do
    DOR.makeDriverReferredByOperator merchantOperatingCityId driverId referredOperatorId

  unless (transporterConfig.requiresOnboardingInspection == Just True) $ Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig driverId True (Just True)
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId "FLEET_CONSENT" Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let title = T.replace "{#fleetOwnerName#}" fleetOwner.firstName merchantPN.title
    let body = T.replace "{#fleetOwnerName#}" fleetOwner.firstName merchantPN.body
    TN.notifyDriver merchantOperatingCityId merchantPN.fcmNotificationType title body driver driver.deviceToken
  pure Success

driverRequestLockKey :: Text -> Text
driverRequestLockKey reqId = "Driver:Request:Id-" <> reqId

createAlertRequest :: Id Person -> Text -> Text -> Text -> AlertRequestData -> TripTransaction -> Flow (Id AlertRequest)
createAlertRequest driverId requesteeId title body requestData tripTransaction = do
  alertRequestId <- triggerAlertRequest driverId requesteeId title body requestData True tripTransaction
  QTT.updateEndRideApprovalRequestId (Just alertRequestId) tripTransaction.id
  pure alertRequestId

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
