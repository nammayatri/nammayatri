{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Driver
  ( getDriverFleetAccessList,
    postDriverFleetAccessSelect,
    postDriverFleetV2AccessSelect,
    postDriverFleetAddVehicle,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
    postDriverFleetUnlink,
    postDriverFleetRemoveVehicle,
    postDriverFleetRemoveDriver,
    getDriverFleetTotalEarning,
    getDriverFleetVehicleEarning,
    getDriverFleetDriverEarning,
    getDriverFleetBookings,
    getDriverFleetAssignments,
    getDriverFleetDriverVehicleAssociation,
    getDriverFleetDriverAssociation,
    getDriverFleetVehicleAssociation,
    getDriverFleetDriverListStats,
    postDriverFleetVehicleDriverRcStatus,
    postDriverUpdateFleetOwnerInfo,
    getDriverFleetOwnerInfo,
    postDriverFleetSendJoiningOtp,
    postDriverFleetVerifyJoiningOtp,
    postDriverFleetLinkRCWithDriver,
    getDriverFleetGetDriverRequests,
    postDriverFleetRespondDriverRequest,
    postDriverFleetAddVehicles,
    postDriverDashboardFleetWmbTripEnd,
    postDriverFleetAddDrivers,
    getDriverFleetGetAllBadge,
    getDriverFleetRoutes,
    getDriverFleetPossibleRoutes,
    postDriverFleetTripPlanner,
    getDriverFleetTripTransactions,
    postDriverFleetAddDriverBusRouteMapping,
    postDriverDashboardFleetTrackDriver,
    castDriverStatus,
    getDriverFleetWmbRouteDetails,
    postDriverFleetGetNearbyDrivers,
    getDriverDashboardInternalHelperGetFleetOwnerId,
    getDriverDashboardInternalHelperGetFleetOwnerIds,
    getDriverFleetStatus,
    postDriverFleetV2AccessMultiOwnerIdSelect,
    validateOperatorToFleetAssoc,
    validateRequestorRoleAndGetEntityId,
    getDriverFleetOperatorInfo,
    checkRCAssociationForFleet,
    postDriverFleetLocationList,
    postDriverFleetGetDriverDetails,
    getDriverFleetDriverDetails,
    postDriverFleetGetNearbyDriversV2,
    getDriverFleetDashboardAnalyticsAllTime,
    getDriverFleetDashboardAnalytics,
    getDriverDashboardFleetTripWaypoints,
    postDriverDashboardFleetEstimateRoute,
    postDriverFleetTripTransactionsV2,
    postDriverFleetApproveDriver,
    postDriverFleetDriverUpdate,
    getDriverFleetVehicleListStats,
    getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles,
    getDriverFleetScheduledBookingList,
    postDriverFleetScheduledBookingAssign,
    postDriverFleetScheduledBookingCancel,
    postDriverFleetDashboardAnalyticsCache,
    postDriverAddRidePayoutAccountNumber,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Endpoints.Driver as Common
import "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride (CancellationReasonCode (..))
import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import Control.Applicative (liftA2, optional)
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Driver as Common
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.Csv
import Data.List (groupBy, nub, sortOn)
import Data.List.NonEmpty (fromList, toList)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DRegV2
import qualified Domain.Action.Dashboard.Management.Driver as DDriver
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration as DRBReg
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.Driver as UIDriver
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.FleetDriverAssociation as FDA
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Action.UI.WMB as DWMB
import qualified Domain.Types as DTC
import qualified Domain.Types.Alert as DTA
import Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.AlertRequest as DTR
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Common as DrInfo
import qualified Domain.Types.DocumentVerificationConfig as DDoc
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverLocation as DDL
import Domain.Types.DriverRCAssociation
import qualified Domain.Types.DriverRidePayoutBankAccount as DRPB
import qualified Domain.Types.FleetBadge as DFB
import qualified Domain.Types.FleetBadgeType as DFBT
import Domain.Types.FleetBookingInformation ()
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.FleetDriverAssociation
import Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as TR
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.TransporterConfig as DTCConfig
import Domain.Types.TripTransaction
import qualified Domain.Types.TripTransaction as DTT
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.VehicleRouteMapping as DVRM
import qualified Domain.Types.VehicleVariant as DV
import Environment
import EulerHS.Prelude (whenNothing_, (<|>))
import Kernel.Beam.Functions as B
import Kernel.External.Maps.HasCoordinates
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude hiding (toList)
import Kernel.Sms.Config
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverFlowStatus as SDF
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LTST
import SharedLogic.Fleet (getFleetOwnerId, getFleetOwnerIds, getFleetOwnersInfoMerchantBased)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.MobileNumberValidation as MobileValidation
import qualified SharedLogic.WMB as WMB
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.FleetBadgeAssociation as CFBA
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Clickhouse.DriverEdaKafka as CHDriverEda
import qualified Storage.Clickhouse.FleetDriverAssociation as CFDA
import qualified Storage.Clickhouse.FleetOperatorDailyStats as CFODS
import qualified Storage.Clickhouse.FleetRcDailyStats as CFRDSExtra
import qualified Storage.Clickhouse.Person as CHPerson
import qualified Storage.Clickhouse.Ride as CQRide
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner)
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.AlertRequest as QAR
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOperatorAssociation as DOV
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverRCAssociationExtra as DRCAE
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverRidePayoutBankAccount as QDRPB
import qualified Storage.Queries.FleetBadge as QFB
import qualified Storage.Queries.FleetBadgeAssociation as QFBA
import qualified Storage.Queries.FleetBookingAssignments as QFBA
import qualified Storage.Queries.FleetBookingInformation as QFBI
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDAExtra
import qualified Storage.Queries.FleetMemberAssociation as FMA
import qualified Storage.Queries.FleetOperatorAssociation as FOV
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperatorAssociation
import qualified Storage.Queries.FleetOperatorDailyStatsExtra as QFODSExtra
import qualified Storage.Queries.FleetOperatorStats as QFleetOperatorStats
import qualified Storage.Queries.FleetOwnerInformation as FOI
import Storage.Queries.FleetRCAssociationExtra as FRAE
import Storage.Queries.FleetRcDailyStatsExtra as QFRDSExtra
import qualified Storage.Queries.FleetRouteAssociation as QFRA
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.RideExtra as QRideExtra
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
import qualified Storage.Queries.TripAlertRequest as QTAR
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import qualified Storage.Queries.VehicleRouteMapping as VRM
import qualified Tools.Csv as Csv
import Tools.Encryption
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Notifications as Notification
import Tools.SMS as Sms hiding (Success)

postDriverFleetAddVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Common.Role ->
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverFleetAddVehicle = postDriverFleetAddVehicleHelper False

type IsBulkUpload = Bool

postDriverFleetAddVehicleHelper ::
  IsBulkUpload ->
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Common.Role ->
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverFleetAddVehicleHelper isBulkUpload merchantShortId opCity reqDriverPhoneNo requestorId mbFleetOwnerId mbMobileCountryCode mbRole req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  let mobileCountryCode = fromMaybe (P.getCountryMobileCode merchantOpCity.country) mbMobileCountryCode
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  MobileValidation.validateMobileNumber transporterConfig reqDriverPhoneNo mobileCountryCode merchantOpCity.country
  phoneNumberHash <- getDbHash reqDriverPhoneNo
  let role = case mbRole of
        Just Common.FLEET -> DP.FLEET_OWNER
        _ -> DP.DRIVER
  entityDetails <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneNumberHash merchant.id role >>= fromMaybeM (DriverNotFound reqDriverPhoneNo)
  let merchantId = entityDetails.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == entityDetails.merchantOperatingCityId) $ throwError (PersonDoesNotExist entityDetails.id.getId)
  (getEntityData, getMbFleetOwnerId) <- checkEnitiesAssociationValidation requestorId mbFleetOwnerId entityDetails merchant.fleetOwnerEnabledCheck
  rc <- RCQuery.findLastVehicleRCWrapper req.registrationNo
  case (getEntityData.role, getMbFleetOwnerId) of
    (DP.DRIVER, Nothing) -> do
      -- DCO case
      void $ checkRCAssociationForDriver getEntityData.id rc True
      void $ DCommon.runVerifyRCFlow getEntityData.id merchant merchantOpCityId opCity req True isBulkUpload Nothing -- Pass fleet.id if addvehicle under fleet or pass driver.id if addvehcile under driver
      logTagInfo "dashboard -> addVehicleUnderDCO : " (show getEntityData.id)
      pure Success
    (_, Just fleetOwnerId) -> do
      -- fleet and fleetDriver case
      whenJust rc $ \rcert -> checkRCAssociationForFleet fleetOwnerId rcert
      void $ DCommon.runVerifyRCFlow getEntityData.id merchant merchantOpCityId opCity req True isBulkUpload (Just $ Id @DP.Person fleetOwnerId) -- Pass fleet.id if addvehicle under fleet or pass driver.id if addvehcile under driver
      let logTag = case getEntityData.role of
            DP.FLEET_OWNER -> "dashboard -> addVehicleUnderFleet"
            DP.DRIVER -> "dashboard -> addVehicleUnderFleetDriver"
            _ -> "dashboard -> addVehicleUnderUnknown"
      logTagInfo logTag (show getEntityData.id)
      pure Success
    _ -> throwError (InvalidRequest "Invalid Data")

checkRCAssociationForDriver :: Id DP.Person -> Maybe DVRC.VehicleRegistrationCertificate -> Bool -> Flow Bool
checkRCAssociationForDriver driverId mbVehicleRC checkFleet = maybe checkAssociationWithDriver checkAssociationWithDriverAndVehicle mbVehicleRC
  where
    checkAssociationWithDriverAndVehicle :: DVRC.VehicleRegistrationCertificate -> Flow Bool
    checkAssociationWithDriverAndVehicle vehicleRC = do
      when (isJust vehicleRC.fleetOwnerId && checkFleet) $ throwError VehicleBelongsToFleet
      now <- getCurrentTime
      allAssociations <- DRCAE.findValidAssociationsForDriverOrRC driverId vehicleRC.id now
      let exactMatch = find (\assoc -> assoc.driverId == driverId && assoc.rcId == vehicleRC.id) allAssociations
          rcAssociations = filter (\assoc -> assoc.rcId == vehicleRC.id && assoc.driverId /= driverId && assoc.isRcActive) allAssociations
          driverAssociations = filter (\assoc -> assoc.driverId == driverId && assoc.rcId /= vehicleRC.id && assoc.isRcActive) allAssociations
      if (isJust exactMatch)
        then return True
        else do
          unless (null rcAssociations) $ throwError VehicleAlreadyLinkedToAnotherDriver
          unless (null driverAssociations) $ throwError DriverAlreadyLinkedToAnotherVehicle
          return False

    checkAssociationWithDriver :: Flow Bool
    checkAssociationWithDriver = do
      isDriverAssociated <- QRCAssociation.findActiveAssociationByDriver driverId True
      when (isJust isDriverAssociated) $ throwError DriverAlreadyLinkedToAnotherVehicle
      return False

checkEnitiesAssociationValidation :: Text -> Maybe Text -> DP.Person -> Maybe Bool -> Flow (DP.Person, Maybe Text)
checkEnitiesAssociationValidation requestorId mbFleetOwnerId entityDetails fleetOwnerEnabledCheck = do
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)

  case requestedPerson.role of
    -- Fleet add vehcile him or under FleetDriver (Driver who has active association with fleet)
    DP.FLEET_OWNER -> do
      fleetOwnerId <- case mbFleetOwnerId of -- Have to discuss
        Nothing -> DCommon.checkFleetOwnerVerification requestorId fleetOwnerEnabledCheck >> pure requestorId
        Just val -> if requestorId == val then pure val else throwError AccessDenied
      handleFleetOwnerFlow fleetOwnerId

    -- Operator should add vehcile under DCO (Driver who independent from fleet), Fleet, FleetDriver (Driver who has active association with fleet)
    DP.OPERATOR -> do
      case mbFleetOwnerId of
        Nothing -> handleOperatorToDriverAndFleet requestedPerson entityDetails
        Just fleetOwnerId -> do
          validateOperatorToFleetAssoc requestedPerson.id.getId fleetOwnerId
          handleFleetOwnerFlow fleetOwnerId
    _ -> throwError (InvalidRequesterRole $ show requestedPerson.role)
  where
    handleFleetOwnerFlow :: Text -> Flow (DP.Person, Maybe Text)
    handleFleetOwnerFlow fleetOwnerId =
      case entityDetails.role of
        DP.FLEET_OWNER -> do
          -- Under Fleet
          unless (fleetOwnerId == entityDetails.id.getId) $
            throwError (InvalidFleetOwner fleetOwnerId)
          pure (entityDetails, Just fleetOwnerId)
        DP.DRIVER -> do
          -- Under FleetDriver
          validateFleetDriverAssociation fleetOwnerId entityDetails.id
          pure (entityDetails, Just fleetOwnerId)
        _ -> throwError (InvalidRoleForVehicleAdd $ show entityDetails.role)

    handleOperatorToDriverAndFleet :: DP.Person -> DP.Person -> Flow (DP.Person, Maybe Text)
    handleOperatorToDriverAndFleet operatorPerson targetPerson = do
      case targetPerson.role of
        DP.DRIVER -> do
          -- Under DCO
          verifyAndAssociateDriver targetPerson.id operatorPerson.id.getId
          pure (targetPerson, Nothing)
        DP.FLEET_OWNER -> do
          -- Under Fleet
          validateOperatorToFleetAssoc operatorPerson.id.getId targetPerson.id.getId
          pure (targetPerson, Just targetPerson.id.getId)
        _ -> throwError (InvalidRoleForVehicleAdd $ show targetPerson.role)

    -- TODO: We should add notify logic in this function. Have to discuss
    verifyAndAssociateDriver :: Id DP.Person -> Text -> Flow ()
    verifyAndAssociateDriver driverId operatorId = do
      isFleetAssociated <- FDV.findByDriverId driverId True
      when (isJust isFleetAssociated) $
        throwError $ DriverHasActiveLink driverId.getId

      isOperatorAssociated <- DOV.findByDriverId driverId True
      case isOperatorAssociated of
        Just associateData ->
          when (associateData.operatorId /= operatorId) $
            throwError $ DriverHasActiveLink driverId.getId
        Nothing -> throwError $ InvalidRequest "Driver has no active link with any entity"

validateOperatorToFleetAssoc :: Text -> Text -> Flow ()
validateOperatorToFleetAssoc operatorId fleetOwnerId = do
  isAssociated <- FOV.findByFleetIdAndOperatorId fleetOwnerId operatorId True
  when (isNothing isAssociated) $
    throwError (FleetNotActiveInOperator fleetOwnerId operatorId)

validateFleetDriverAssociation :: Text -> Id DP.Person -> Flow ()
validateFleetDriverAssociation fleetOwnerId personId = do
  isAssociated <- FDV.findByDriverIdAndFleetOwnerId personId fleetOwnerId True
  when (isNothing isAssociated) $
    throwError (DriverNotActiveInFleet personId.getId fleetOwnerId)

checkRCAssociationForFleet :: Text -> DVRC.VehicleRegistrationCertificate -> Flow ()
checkRCAssociationForFleet fleetOwnerId vehicleRC = do
  when (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId /= Just fleetOwnerId) $ throwError VehicleBelongsToAnotherFleet
  activeAssociationsOfRC <- DRCAE.findAllActiveAssociationByRCId vehicleRC.id
  let rcAssociatedDriverIds = map (.driverId) activeAssociationsOfRC
  forM_ rcAssociatedDriverIds $ \driverId -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
    when (isNothing isFleetDriver) $ throwError (VehicleLinkedToInvalidDriver)

---------------------------------------------------------------------

getDriverFleetGetDriverRequests :: ShortId DM.Merchant -> Context.City -> Maybe UTCTime -> Maybe UTCTime -> Maybe DTA.AlertRequestType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe AlertRequestStatus -> Maybe Int -> Maybe Int -> Flow Common.DriverRequestRespT
getDriverFleetGetDriverRequests merchantShortId opCity mbFrom mbTo mbAlertRequestType mbRouteCode mbDriverId mbBadgeName mbFleetOwnerId mbalertStatus mbLimit mbOffset = do
  memberPersonId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "Member person id is required")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId Nothing
  let fleetOwnerIds = map fst fleetOwnerInfo
      fleetNameMap = Map.fromList fleetOwnerInfo
  tripAlertRequests <-
    case mbBadgeName of
      Just badgeName -> do
        badges <- QFB.findAllBadgesByNameAndBadgeTypeAndFleetOwnerIds (Id <$> fleetOwnerIds) badgeName DFBT.DRIVER
        let fleetBadgeIds = map (.id) badges
        B.runInReplica $ QTAR.findTripAlertRequestsByFleetOwnerIds merchantOpCityId fleetOwnerIds mbFrom mbTo mbAlertRequestType mbDriverId (Just fleetBadgeIds) mbRouteCode mbalertStatus mbLimit mbOffset
      Nothing -> B.runInReplica $ QTAR.findTripAlertRequestsByFleetOwnerIds merchantOpCityId fleetOwnerIds mbFrom mbTo mbAlertRequestType mbDriverId Nothing mbRouteCode mbalertStatus mbLimit mbOffset
  driverRequestList <-
    mapM
      ( \tripAlertRequest -> do
          alertRequest <- QAR.findByPrimaryKey tripAlertRequest.alertRequestId >>= fromMaybeM (DriverRequestNotFound tripAlertRequest.alertRequestId.getId)
          let fleetOwnerName = fromMaybe "" (Map.lookup tripAlertRequest.fleetOwnerId.getId fleetNameMap)
          buildDriverRequestListItem tripAlertRequest.fleetOwnerId.getId fleetOwnerName tripAlertRequest.tripTransactionId tripAlertRequest.driverId tripAlertRequest.routeCode tripAlertRequest.isViolated alertRequest
      )
      tripAlertRequests
  pure $ Common.DriverRequestRespT driverRequestList
  where
    buildDriverRequestListItem fleetOwnerId fleetOwnerName tripTransactionId driverId routeCode isViolated DTR.AlertRequest {..} = do
      pure $
        Common.DriverRequestDetailsT
          { raisedAt = createdAt,
            approvalRequestId = id.getId,
            driverId = driverId.getId,
            tripTransactionId = tripTransactionId.getId,
            isViolated = isViolated,
            status = Just status,
            ..
          }

---------------------------------------------------------------------
postDriverFleetRespondDriverRequest ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.RequestRespondReq ->
  Flow APISuccess
postDriverFleetRespondDriverRequest merchantShortId opCity fleetOwnerId req = do
  Redis.whenWithLockRedis (DWMB.driverRequestLockKey req.approvalRequestId) 60 $ do
    merchant <- findMerchantByShortId merchantShortId
    merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
    driverRequest <- B.runInReplica $ QAR.findByPrimaryKey (Id req.approvalRequestId) >>= fromMaybeM (DriverRequestNotFound req.approvalRequestId)
    driver <- B.runInReplica $ QPerson.findById driverRequest.requestorId >>= fromMaybeM (PersonDoesNotExist driverRequest.requestorId.getId)
    case driverRequest.status of
      DTA.AWAITING_APPROVAL -> WMB.updateAlertRequestStatus req.status (Just req.reason) (Id req.approvalRequestId)
      _ -> throwError $ RequestAlreadyProcessed driverRequest.id.getId
    void $ case req.status of
      DTA.REJECTED -> Notification.requestRejectionNotification merchantOpCityId notificationTitle (message driverRequest) driver driver.deviceToken driverRequest{status = DTA.REJECTED, reason = Just req.reason}
      DTA.ACCEPTED -> do
        case driverRequest.requestData of
          DTA.EndRide DTA.EndRideData {..} -> do
            fleetConfig <- QFC.findByPrimaryKey (Id fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetOwnerId)
            tripTransaction <- QTT.findByTransactionId (Id tripTransactionId) >>= fromMaybeM (TripTransactionNotFound tripTransactionId)
            void $ WMB.endOngoingTripTransaction fleetConfig tripTransaction (LatLong lat lon) DriverOnApproval False
          _ -> pure ()
      _ -> pure ()
  pure Success
  where
    notificationTitle = "Request Rejected!"
    message req_ =
      cs $
        unwords
          [ "Sorry, your request for",
            (show $ DTA.castAlertRequestDataToRequestType req_.requestData) <> " has been rejected",
            "Check the app for more details."
          ]

---------------------------------------------------------------------
postDriverFleetAddRCWithoutDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.RegisterRCReq ->
  Flow APISuccess
postDriverFleetAddRCWithoutDriver merchantShortId opCity fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  let personId = Id fleetOwnerId :: Id DP.Person
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist driver.id.getId)
  rc <- RCQuery.findLastVehicleRCWrapper req.vehicleRegistrationCertNumber
  whenJust rc $ \rcert -> checkRCAssociationForFleet fleetOwnerId rcert
  let rcReq =
        DomainRC.DriverRCReq
          { vehicleRegistrationCertNumber = req.vehicleRegistrationCertNumber,
            imageId = cast req.imageId,
            operatingCity = req.operatingCity,
            dateOfRegistration = req.dateOfRegistration,
            airConditioned = req.airConditioned,
            oxygen = req.oxygen,
            ventilator = req.ventilator,
            vehicleCategory = req.vehicleCategory,
            vehicleDetails = Nothing,
            isRCImageValidated = Nothing
          }
  void $ DomainRC.verifyRC False (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq False (Just personId)
  logTagInfo "dashboard -> Register RC For Fleet : " (show driver.id)
  pure Success

---------------------------------------------------------------------

getDriverFleetGetAllVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Bool ->
  Flow Common.ListVehicleResT
getDriverFleetGetAllVehicle merchantShortId _ mbLimit mbOffset mbRegNumberString mbFleetOwnerId mbIsActive mbMemberPersonId mbSendDriverMobileNumber = do
  memberPersonId <- mbMemberPersonId & fromMaybeM (InvalidRequest "Member person id is required")
  let sendDriverMobileNumber = fromMaybe False mbSendDriverMobileNumber
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  merchant <- findMerchantByShortId merchantShortId
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId mbFleetOwnerId
  let fleetNameMap = Map.fromList fleetOwnerInfo
      fleetOwnerIds = map fst fleetOwnerInfo
  when (merchant.fleetOwnerEnabledCheck == Just True) $
    forM_ fleetOwnerIds $ \foId -> DCommon.checkFleetOwnerVerification foId merchant.fleetOwnerEnabledCheck
  mbRegNumberStringHash <- mapM getDbHash mbRegNumberString
  case mbIsActive of
    Just True -> do
      activeVehicleList <- QRCAssociation.findAllActiveAssociationByFleetOwnerIds fleetOwnerIds (Just limit) (Just offset) mbRegNumberString mbRegNumberStringHash
      vehicles <- traverse (convertToVehicleAPIEntityTFromAssociation sendDriverMobileNumber fleetNameMap) activeVehicleList
      return $ Common.ListVehicleResT (catMaybes vehicles)
    Just False -> do
      inactiveVehicleList <- QRCAssociation.findAllInactiveAssociationByFleetOwnerIds fleetOwnerIds limit offset mbRegNumberString mbRegNumberStringHash
      vehicles <- traverse (convertToVehicleAPIEntityTFromAssociation sendDriverMobileNumber fleetNameMap) inactiveVehicleList
      return $ Common.ListVehicleResT (catMaybes vehicles)
    Nothing -> do
      vehicleList <- RCQuery.findAllValidRcByFleetOwnerIdsAndSearchString (toInteger limit) (toInteger offset) merchant.id fleetOwnerIds mbRegNumberString mbRegNumberStringHash
      vehicles <- traverse (convertToVehicleAPIEntityT sendDriverMobileNumber fleetNameMap) vehicleList
      return $ Common.ListVehicleResT (catMaybes vehicles)

convertToVehicleAPIEntityTFromAssociation :: Bool -> Map.Map Text Text -> (DriverRCAssociation, DVRC.VehicleRegistrationCertificate) -> Flow (Maybe Common.VehicleAPIEntityT)
convertToVehicleAPIEntityTFromAssociation sendDriverMobileNumber fleetNameMap (association, rc) = do
  certificateNumber' <- decrypt rc.certificateNumber
  (driverMobileNumber, driverMobileCountryCode) <- getDriverDetailsFromRC sendDriverMobileNumber rc.id
  pure $
    rc.fleetOwnerId >>= \foId ->
      Just $
        Common.VehicleAPIEntityT
          { variant = DCommon.castVehicleVariantDashboard rc.vehicleVariant,
            model = rc.vehicleModel,
            color = rc.vehicleColor,
            registrationNo = certificateNumber',
            isActive = (Just association.isRcActive),
            fleetOwnerId = foId,
            fleetOwnerName = fromMaybe "" (Map.lookup foId fleetNameMap),
            driverMobileNumber,
            driverMobileCountryCode
          }

convertToVehicleAPIEntityT :: Bool -> Map.Map Text Text -> DVRC.VehicleRegistrationCertificate -> Flow (Maybe Common.VehicleAPIEntityT)
convertToVehicleAPIEntityT sendDriverMobileNumber fleetNameMap DVRC.VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  mActiveAssociation <- QRCAssociation.findActiveAssociationByRC id True
  let isActive = isJust mActiveAssociation
  (driverMobileNumber, driverMobileCountryCode) <- getDriverDetailsFromRC sendDriverMobileNumber id
  pure $
    fleetOwnerId >>= \foId ->
      Just $
        Common.VehicleAPIEntityT
          { variant = DCommon.castVehicleVariantDashboard vehicleVariant,
            model = vehicleModel,
            color = vehicleColor,
            registrationNo = certificateNumber',
            isActive = (Just isActive),
            fleetOwnerId = foId,
            fleetOwnerName = fromMaybe "" (Map.lookup foId fleetNameMap),
            driverMobileNumber,
            driverMobileCountryCode
          }

getDriverDetailsFromRC :: Bool -> Id DVRC.VehicleRegistrationCertificate -> Flow (Maybe Text, Maybe Text)
getDriverDetailsFromRC sendDriverMobileNumber rcId = do
  if sendDriverMobileNumber
    then do
      mbRCAccountNumber <- QDRPB.findByRcId rcId
      case mbRCAccountNumber of
        Just rcAccountNumber -> do
          person <- QPerson.findById rcAccountNumber.driverId >>= fromMaybeM (PersonDoesNotExist rcAccountNumber.driverId.getId)
          driverMobileNumber <- decrypt `mapM` person.mobileNumber
          pure (driverMobileNumber, person.mobileCountryCode)
        Nothing -> pure (Nothing, Nothing)
    else pure (Nothing, Nothing)

---------------------------------------------------------------------

getDriverFleetGetAllDriver :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Flow Common.FleetListDriverResT
getDriverFleetGetAllDriver merchantShortId _opCity mblimit mboffset mbMobileNumber mbName mbSearchString mbFleetOwnerId mbIsActive mbMemberPersonId = do
  memberPersonId <- mbMemberPersonId & fromMaybeM (InvalidRequest "Member person id is required")
  merchant <- findMerchantByShortId merchantShortId
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId mbFleetOwnerId
  let fleetOwnerIds = map fst fleetOwnerInfo
      fleetNameMap = Map.fromList fleetOwnerInfo
  when (merchant.fleetOwnerEnabledCheck == Just True) $
    forM_ fleetOwnerIds $ \foId -> DCommon.checkFleetOwnerVerification foId merchant.fleetOwnerEnabledCheck
  let limit = fromMaybe 10 mblimit
      offset = fromMaybe 0 mboffset
  mobileNumberHash <- case mbSearchString of
    Just _ -> pure Nothing
    Nothing -> case mbMobileNumber of
      Just phNo -> Just <$> getDbHash phNo
      Nothing -> pure Nothing
  case mbIsActive of
    Just True -> do
      pairs <- FDV.findAllActiveDriverByFleetOwnerIds fleetOwnerIds (Just limit) (Just offset) mobileNumberHash mbName mbSearchString (Just True)
      fleetDriversInfos <- mapM (convertToDriverAPIEntityT fleetNameMap) pairs
      return $ Common.FleetListDriverResT fleetDriversInfos
    Just False -> do
      pairs <- FDV.findAllInactiveDriverByFleetOwnerIds fleetOwnerIds (Just limit) (Just offset) mobileNumberHash mbName mbSearchString
      fleetDriversInfos <- mapM (convertToDriverAPIEntityT fleetNameMap) pairs
      return $ Common.FleetListDriverResT fleetDriversInfos
    Nothing -> do
      pairs <- FDV.findAllDriverByFleetOwnerIds fleetOwnerIds (Just limit) (Just offset) mobileNumberHash mbName mbSearchString
      fleetDriversInfos <- mapM (convertToDriverAPIEntityT fleetNameMap) pairs
      return $ Common.FleetListDriverResT fleetDriversInfos

convertToDriverAPIEntityT :: Map.Map Text Text -> (FleetDriverAssociation, DP.Person) -> Flow Common.FleetDriversAPIEntityT
convertToDriverAPIEntityT fleetNameMap (association, person) = do
  unencryptedMobileNumber <- mapM decrypt person.mobileNumber
  vehicle <- QVehicle.findById person.id
  let isActive = isJust vehicle
  pure $
    Common.FleetDriversAPIEntityT
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = unencryptedMobileNumber,
        mobileCountryCode = person.mobileCountryCode,
        isActive = Just isActive,
        fleetOwnerId = association.fleetOwnerId,
        fleetOwnerName = fromMaybe "" (Map.lookup association.fleetOwnerId fleetNameMap)
      }

---------------------------------------------------------------------
postDriverFleetUnlink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Text ->
  Maybe Text ->
  Flow APISuccess
postDriverFleetUnlink merchantShortId opCity requestorId reqDriverId vehicleNo mbFleetOwnerId = do
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  case entityRole of
    DP.FLEET_OWNER -> do
      DCommon.checkFleetOwnerVerification entityId merchant.fleetOwnerEnabledCheck
      isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId personId entityId True
      case isFleetDriver of
        Nothing -> throwError DriverNotPartOfFleet
        Just fleetDriver -> do
          unless fleetDriver.isActive $ throwError DriverNotActiveWithFleet
      unlinkVehicleFromDriver merchant personId vehicleNo opCity DP.FLEET_OWNER
    DP.OPERATOR -> do
      isDriverOperator <- DDriver.checkDriverOperatorAssociation personId (Id entityId)
      when (not isDriverOperator) $ throwError DriverNotPartOfOperator
      unlinkVehicleFromDriver merchant personId vehicleNo opCity DP.OPERATOR
    _ -> throwError $ InvalidRequest "Invalid Data"
  pure Success

unlinkVehicleFromDriver :: DM.Merchant -> Id DP.Person -> Text -> Context.City -> DP.Role -> Flow ()
unlinkVehicleFromDriver merchant personId vehicleNo opCity role = do
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbVehicle <- QVehicle.findById driver.id
  let isNotVipOfficer = maybe True ((/=) DV.VIP_OFFICER . (.variant)) mbVehicle
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)
  rc <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (RCNotFound vehicleNo)
  driverInfo <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (transporterConfig.deactivateRCOnUnlink == Just True) $ DomainRC.deactivateCurrentRC transporterConfig personId
  when ((driverInfo.onboardingVehicleCategory /= Just DVC.BUS && isNotVipOfficer) && transporterConfig.disableDriverWhenUnlinkingVehicle == Just True) $ Analytics.updateEnabledVerifiedStateWithAnalytics (Just driverInfo) transporterConfig personId False (Just False)
  _ <- QRCAssociation.endAssociationForRC personId rc.id
  logTagInfo (show role <> " -> unlinkVehicle : ") (show personId)

---------------------------------------------------------------------
postDriverFleetRemoveVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Maybe Text ->
  Flow APISuccess
postDriverFleetRemoveVehicle merchantShortId _ fleetOwnerId_ vehicleNo mbRequestorId = do
  void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId_
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId_ merchant.fleetOwnerEnabledCheck
  vehicle <- QVehicle.findByRegistrationNo vehicleNo
  whenJust vehicle $ \veh -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId veh.driverId fleetOwnerId_ True
    when (isJust isFleetDriver) $ throwError (InvalidRequest "Vehicle is linked to a fleet driver, please unlink the vehicle from the driver before removing it.")
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (VehicleDoesNotExist vehicleNo)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId_) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId_)
  associations <- QRCAssociation.findAllActiveAssociationByRCId vehicleRC.id ----- Here ending all the association of the vehicle with the fleet drivers
  forM_ associations $ \assoc -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId assoc.driverId fleetOwnerId_ True
    when (isJust isFleetDriver) $ QRCAssociation.endAssociationForRC assoc.driverId vehicleRC.id
  RCQuery.upsert (updatedVehicleRegistrationCertificate vehicleRC)
  FRAE.endAssociationForRC (Id fleetOwnerId_ :: Id DP.Person) vehicleRC.id
  pure Success
  where
    updatedVehicleRegistrationCertificate DVRC.VehicleRegistrationCertificate {..} = DVRC.VehicleRegistrationCertificate {fleetOwnerId = Nothing, ..}

postDriverAddRidePayoutAccountNumber ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.AddRidePayoutAccountNumberReq ->
  Flow APISuccess
postDriverAddRidePayoutAccountNumber merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  now <- getCurrentTime
  mobileNumberHash <- getDbHash req.driverMobileNumber
  mbPerson <- QP.findByMobileNumberAndMerchantAndRole req.driverMobileNumberCountryCode mobileNumberHash merchant.id DP.DRIVER
  person <-
    case mbPerson of
      Nothing -> do
        void $ DRBReg.auth merchantShortId opCity (Common.AuthReq req.driverMobileNumber req.driverMobileNumberCountryCode)
        QP.findByMobileNumberAndMerchantAndRole req.driverMobileNumberCountryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (DriverNotFound req.driverMobileNumber)
      Just p -> return p
  case (req.accountNumber, req.ifscCode) of
    (Just accountNumber, Just ifscCode) -> do
      bankAccountNumber <- encrypt accountNumber
      bankIfscCode <- encrypt ifscCode
      rc <- RCQuery.findLastVehicleRCWrapper req.vehicleRegistrationNumber >>= fromMaybeM (RCNotFound req.vehicleRegistrationNumber)
      mbAlreadyExists <- QDRPB.findByRcId rc.id
      whenJust mbAlreadyExists $ \_ -> do
        throwError (InvalidRequest "Ride Payout Account Number already exists")
      id <- generateGUID
      let driverRidePayoutBankAccount =
            DRPB.DriverRidePayoutBankAccount
              { bankAccountNumber = bankAccountNumber,
                bankIfscCode = bankIfscCode,
                driverId = person.id,
                id,
                rcId = rc.id,
                merchantId = Just merchant.id,
                merchantOperatingCityId = Just merchantOpCity.id,
                createdAt = now,
                updatedAt = now
              }
      let payoutVpa = accountNumber <> "@" <> ifscCode <> ".ifsc.npci"
      QDriverInfo.updatePayoutVpaAndStatusByDriverIds (Just payoutVpa) (Just DI.MANUALLY_ADDED) [person.id]
      QDRPB.create driverRidePayoutBankAccount
      pure Success
    _ -> pure Success

---------------------------------------------------------------------
postDriverFleetAddVehicles ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreateVehiclesReq ->
  Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddVehicles merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  requestorId <- case req.requestorId of
    Nothing -> throwError $ FleetOwnerOrOperatorIdRequired
    Just id -> pure id
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  rcReq <-
    Csv.readCsv @VehicleDetailsCSVRow @(Common.RegisterRCReq, DbHash, [DRoute.Route], Maybe Text, Maybe Text, Maybe Text) req.file $
      parseVehicleInfo merchantOpCity
  when (length rcReq > 100) $ throwError $ MaxVehiclesLimitExceeded 100 -- TODO: Configure the limit
  case requestedPerson.role of
    DP.FLEET_OWNER -> do
      fleetOwnerId <- maybe (pure requestorId) (\val -> if requestorId == val then pure requestorId else throwError AccessDenied) req.fleetOwnerId
      DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
      unprocessedVehicleRouteMappingEntities <- case transporterConfig.requireRouteMappingInVehicle of
        False -> do
          pure []
        True -> do
          foldlM
            ( \unprocessedEntities (registerRcReq, vehicleNumberHash, routes, _, _, _) -> do
                unprocessedVehicleRouteMapping <-
                  mapM
                    ( \route -> do
                        VRM.findOneMapping vehicleNumberHash route.code
                          >>= \case
                            Just vehicleRouteMapping ->
                              if not vehicleRouteMapping.blocked && vehicleRouteMapping.fleetOwnerId.getId /= fleetOwnerId
                                then pure $ Just ("Vehicle Route Mapping for Vehicle: " <> registerRcReq.vehicleRegistrationCertNumber <> ", Route: " <> route.code <> "is linked to another fleet, please delink and try again.")
                                else do
                                  withTryCatch
                                    "buildVehicleRouteMapping"
                                    (buildVehicleRouteMapping (Id fleetOwnerId) merchant.id merchantOpCity.id registerRcReq.vehicleRegistrationCertNumber route)
                                    >>= \case
                                      Left err -> return $ Just ("Failed to Add Vehicle Route Mapping for Vehicle: " <> registerRcReq.vehicleRegistrationCertNumber <> ", Route: " <> route.code <> ", Error: " <> (T.pack $ displayException err))
                                      Right _ -> pure Nothing
                            Nothing -> do
                              withTryCatch
                                "buildVehicleRouteMapping"
                                (buildVehicleRouteMapping (Id fleetOwnerId) merchant.id merchantOpCity.id registerRcReq.vehicleRegistrationCertNumber route)
                                >>= \case
                                  Left err -> return $ Just ("Failed to Add Vehicle Route Mapping for Vehicle: " <> registerRcReq.vehicleRegistrationCertNumber <> ", Route: " <> route.code <> ", Error: " <> (T.pack $ displayException err))
                                  Right _ -> pure Nothing
                    )
                    routes
                return $ unprocessedEntities <> catMaybes unprocessedVehicleRouteMapping
            )
            []
            rcReq

      unprocessedRCAdditionEntities <-
        foldlM
          ( \unprocessedEntities (registerRcReq, _, _, mbCountryCode, mbFleetNo, mbDriverNo) -> do
              decryptedMobileNumber <- mapM decrypt requestedPerson.mobileNumber
              case decryptedMobileNumber of
                Just mobileNumber -> do
                  currProcessEntity <- case (mbFleetNo, mbDriverNo) of
                    (Nothing, Nothing) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq mobileNumber Nothing (Just Common.FLEET) merchantOpCity -- Add vehicles under requested Fleet
                    (Nothing, Just driverNo) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq driverNo (Just mobileNumber) (Just Common.DRIVER) merchantOpCity -- Map driver <-> vehicle under requested fleet
                    (_, _) -> pure $ Left $ "Unable to add Vehicle (" <> registerRcReq.vehicleRegistrationCertNumber <> "): Invalid request"
                  case currProcessEntity of
                    Left err -> return $ unprocessedEntities <> [err]
                    Right _ -> return unprocessedEntities
                Nothing -> return $ unprocessedEntities <> ["Person do not have a mobile number " <> requestedPerson.id.getId]
          )
          []
          rcReq
      pure Common.APISuccessWithUnprocessedEntities {unprocessedEntities = unprocessedVehicleRouteMappingEntities <> unprocessedRCAdditionEntities}
    DP.OPERATOR -> do
      unprocessedRCAdditionEntities <-
        foldlM
          ( \unprocessedEntities (registerRcReq, _, _, mbCountryCode, mbFleetNo, mbDriverNo) -> do
              currProcessEntity <- case (mbFleetNo, mbDriverNo) of
                (Nothing, Nothing) -> pure $ Left $ "Unable to add Vehicle (" <> registerRcReq.vehicleRegistrationCertNumber <> "): Neither fleet nor driver phone number provided"
                (Just fleetNo, Nothing) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq fleetNo (Just fleetNo) (Just Common.FLEET) merchantOpCity -- Add vehicles under Fleet
                (Nothing, Just driverNo) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq driverNo Nothing (Just Common.DRIVER) merchantOpCity -- Add vehicles under DCO
                (Just fleetNo, Just driverNo) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq driverNo (Just fleetNo) (Just Common.DRIVER) merchantOpCity -- Map driver <-> vehicle under fleer
              case currProcessEntity of
                Left err -> return $ unprocessedEntities <> [err]
                Right _ -> return unprocessedEntities
          )
          []
          rcReq
      pure Common.APISuccessWithUnprocessedEntities {unprocessedEntities = unprocessedRCAdditionEntities}
    _ -> throwError $ InvalidRequest "Invalid Data"
  where
    handleAddVehicleWithTry ::
      DM.Merchant ->
      DTCConfig.TransporterConfig ->
      Maybe Text ->
      Text ->
      Common.RegisterRCReq ->
      Text ->
      Maybe Text ->
      Maybe Common.Role ->
      DMOC.MerchantOperatingCity ->
      Flow (Either Text ())
    handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq phoneNo mbFleetNo mbRole merchantOpCity = do
      result <- withTryCatch "handleAddVehicleWithTry" $ do
        let mobileCountryCode = fromMaybe (P.getCountryMobileCode merchantOpCity.country) mbCountryCode
        let addVehicleReq = convertToAddVehicleReq registerRcReq
        runRequestValidation Common.validateAddVehicleReq addVehicleReq
        MobileValidation.validateMobileNumber transporterConfig phoneNo mobileCountryCode merchantOpCity.country
        whenJust mbFleetNo $ \fleetNo -> MobileValidation.validateMobileNumber transporterConfig fleetNo mobileCountryCode merchantOpCity.country

        when transporterConfig.enableExistingVehicleInBulkUpload $
          isVehicleAlreadyAssociatedWithFleetOrDriver addVehicleReq.registrationNo

        mbFleetOwnerId <- case mbFleetNo of
          Just fleetNo -> do
            phoneHash <- getDbHash fleetNo
            QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneHash merchant.id DP.FLEET_OWNER >>= fromMaybeM (FleetOwnerNotFound fleetNo) <&> (Just . (.id.getId))
          Nothing -> pure Nothing

        postDriverFleetAddVehicleHelper True merchant.shortId opCity phoneNo requestorId mbFleetOwnerId mbCountryCode mbRole addVehicleReq

      case result of
        Left e -> return $ Left $ "Error: " <> T.pack (displayException e)
        Right _ -> return $ Right ()

    isVehicleAlreadyAssociatedWithFleetOrDriver :: Text -> Flow () -- checking vehicle present in the system or not
    isVehicleAlreadyAssociatedWithFleetOrDriver vehicleNo = do
      rc <- RCQuery.findLastVehicleRCWrapperWithApproved vehicleNo (Just True)
      case rc of
        Nothing -> throwError $ VehicleDoesNotExist vehicleNo
        Just rcert -> do
          case rcert.verificationStatus of
            Documents.VALID -> pure ()
            _ -> throwError $ VehicleNotVerified vehicleNo

    parseVehicleInfo :: DMOC.MerchantOperatingCity -> Int -> VehicleDetailsCSVRow -> Flow (Common.RegisterRCReq, DbHash, [DRoute.Route], Maybe Text, Maybe Text, Maybe Text)
    parseVehicleInfo moc idx row = do
      let airConditioned :: (Maybe Bool) = Csv.readMaybeCSVField idx row.airConditioned "Air Conditioned"
          mbRouteCodes :: Maybe [Text] = Csv.readMaybeCSVField idx (fromMaybe "" row.routeCodes) "Route Codes"
          mbFleetPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.fleetPhoneNo) "Fleet Phone Number"
          mbDriverPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.driverPhoneNo) "Driver Phone Number"
          mbCountryCode :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.countryCode) "Country Code"
          vehicleCategory :: Maybe DVC.VehicleCategory = Csv.readMaybeCSVField idx (fromMaybe "" row.vehicleCategory) "Vehicle Category"
      vehicleRegistrationCertNumber <- Csv.cleanCSVField idx row.registrationNo "Registration No"
      vehicleNumberHash <- getDbHash vehicleRegistrationCertNumber
      routes <-
        case mbRouteCodes of
          Just routeCodes -> mapM (\routeCode -> QRoute.findByRouteCode routeCode >>= fromMaybeM (RouteNotFound routeCode)) routeCodes
          Nothing -> pure []
      pure
        ( Common.RegisterRCReq
            { dateOfRegistration = Nothing,
              oxygen = Nothing,
              ventilator = Nothing,
              operatingCity = show moc.city,
              imageId = Id "bulkVehicleUpload",
              vehicleDetails = Nothing,
              vehicleCategory = Just $ fromMaybe DVC.CAR vehicleCategory,
              ..
            },
          vehicleNumberHash,
          routes,
          mbCountryCode,
          mbFleetPhoneNo,
          mbDriverPhoneNo
        )

    buildVehicleRouteMapping fleetOwnerId merchantId merchantOperatingCityId vehicleRegistrationNumber route = do
      now <- getCurrentTime
      vehicleNumber <- encrypt vehicleRegistrationNumber
      vehicleNumberHash <- getDbHash vehicleRegistrationNumber
      let vehicleRouteMapping =
            DVRM.VehicleRouteMapping
              { blocked = False,
                routeCode = route.code,
                createdAt = now,
                updatedAt = now,
                ..
              }
      VRM.upsert vehicleRouteMapping vehicleNumberHash

data VehicleDetailsCSVRow = VehicleDetailsCSVRow
  { registrationNo :: Text,
    airConditioned :: Text,
    routeCodes :: Maybe Text,
    vehicleCategory :: Maybe Text,
    fleetPhoneNo :: Maybe Text,
    driverPhoneNo :: Maybe Text,
    countryCode :: Maybe Text
  }
  deriving (Show)

instance FromNamedRecord VehicleDetailsCSVRow where
  parseNamedRecord r =
    VehicleDetailsCSVRow
      <$> r .: "registration_no"
      <*> r .: "air_conditioned"
      <*> optional (r .: "route_codes")
      <*> optional (r .: "vehicle_category")
      <*> optional (r .: "fleet_phone_no")
      <*> optional (r .: "driver_phone_no")
      <*> optional (r .: "country_code")

---------------------------------------------------------------------
postDriverFleetRemoveDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Maybe Text ->
  Flow APISuccess
postDriverFleetRemoveDriver merchantShortId opCity requestorId driverId mbFleetOwnerId = do
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let personId = cast @Common.Driver @DP.Person driverId
  case entityRole of
    DP.FLEET_OWNER -> do
      DCommon.checkFleetOwnerVerification entityId merchant.fleetOwnerEnabledCheck
      associationList <- QRCAssociation.findAllLinkedByDriverId personId
      forM_ associationList $ \assoc -> do
        rc <- RCQuery.findByRCIdAndFleetOwnerId assoc.rcId $ Just entityId
        when (isJust rc) $ throwError (InvalidRequest "Driver is linked to fleet Vehicle, first unlink then try")
      -- Check if there's an active association before ending it
      mbActiveAssociation <- FDV.findByDriverIdAndFleetOwnerId personId entityId True
      -- Check if driver has any active rides (not completed or cancelled)
      when (isJust mbActiveAssociation) $ do
        mbActiveRide <- B.runInReplica $ QRideExtra.getUpcomingOrActiveByDriverId personId
        when (isJust mbActiveRide) $ throwError (InvalidRequest "Driver has active rides. Please complete or cancel all rides before removing from fleet")
      FDV.endFleetDriverAssociation entityId personId
      -- Only decrement analytics if there was an active association
      when (isJust mbActiveAssociation) $ do
        Analytics.handleDriverAnalyticsAndFlowStatus
          transporterConfig
          personId
          Nothing
          ( \driverInfo -> do
              Analytics.decrementFleetOwnerAnalyticsActiveDriverCount (Just entityId) personId
              mbOperator <- FOV.findByFleetOwnerId entityId True
              when (isNothing mbOperator) $ logTagError "AnalyticsRemoveDriver" "Operator not found for fleet owner"
              whenJust mbOperator $ \operator -> do
                when driverInfo.enabled $ Analytics.decrementOperatorAnalyticsDriverEnabled transporterConfig operator.operatorId
                Analytics.decrementOperatorAnalyticsActiveDriver transporterConfig operator.operatorId
          )
          ( \driverInfo -> do
              DDriverMode.decrementFleetOperatorStatusKeyForDriver DP.FLEET_OWNER entityId driverInfo.driverFlowStatus
          )
    DP.OPERATOR -> do
      -- Check if there's an active association before ending it
      mbActiveAssociation <- DOV.findByDriverIdAndOperatorId personId (Id entityId) True
      DOV.endOperatorDriverAssociation entityId personId
      -- Only decrement analytics if there was an active association
      when (isJust mbActiveAssociation) $ do
        Analytics.handleDriverAnalyticsAndFlowStatus
          transporterConfig
          personId
          Nothing
          ( \driverInfo -> do
              when driverInfo.enabled $ Analytics.decrementOperatorAnalyticsDriverEnabled transporterConfig entityId
              Analytics.decrementOperatorAnalyticsActiveDriver transporterConfig entityId
          )
          ( \driverInfo -> do
              DDriverMode.decrementFleetOperatorStatusKeyForDriver DP.OPERATOR entityId driverInfo.driverFlowStatus
          )
    _ -> throwError (InvalidRequest "Invalid Data")
  pure Success

validateRequestorRoleAndGetEntityId :: DP.Person -> Maybe Text -> Flow (DP.Role, Text)
validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId = do
  case requestedPerson.role of
    DP.FLEET_OWNER -> do
      -- Fleet Owner tries to do operation
      fleetOwnerid <- maybe (pure requestedPerson.id.getId) (\val -> if requestedPerson.id.getId == val then pure requestedPerson.id.getId else throwError AccessDenied) mbFleetOwnerId
      pure (DP.FLEET_OWNER, fleetOwnerid)
    DP.OPERATOR -> do
      case mbFleetOwnerId of
        Just fleetOwnerId -> do
          -- Operator tries to do operation on behalf of the fleet
          validateOperatorToFleetAssoc requestedPerson.id.getId fleetOwnerId
          pure (DP.FLEET_OWNER, fleetOwnerId)
        Nothing -> pure (DP.OPERATOR, requestedPerson.id.getId) -- Operator tries to do operation
    _ -> throwError (InvalidRequest "Invalid Data")

---------------------------------------------------------------------
getDriverFleetTotalEarning ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.FleetTotalEarningResponse
getDriverFleetTotalEarning _merchantShortId _ fleetOwnerId mbFrom mbTo = do
  merchant <- findMerchantByShortId _merchantShortId
  now <- getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  (totalEarning, totalDistanceTravelled, completedRides, cancelledRides) <- CQRide.totalRidesStatsInFleet (Just fleetOwnerId) from to
  totalVehicle <- VRCQuery.countAllActiveRCForFleet fleetOwnerId merchant.id
  let totalRides = completedRides + cancelledRides
  let conversionRate = if totalRides == 0 then 0 else fromIntegral completedRides / fromIntegral totalRides
  let cancellationRate = if totalRides == 0 then 0 else fromIntegral cancelledRides / fromIntegral totalRides
  pure $ Common.FleetTotalEarningResponse {totalDistanceTravelled = fromIntegral totalDistanceTravelled / 1000.0, totalRides = completedRides, ..}

---------------------------------------------------------------------
getDriverFleetVehicleEarning ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.FleetEarningListRes
getDriverFleetVehicleEarning _merchantShortId _ fleetOwnerId mbVehicleNumber mbLimit mbOffset mbFrom mbTo = do
  now <- getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  merchant <- findMerchantByShortId _merchantShortId
  listOfAllRc <- getListOfVehicles mbVehicleNumber fleetOwnerId mbLimit mbOffset Nothing merchant.id Nothing Nothing
  res <- forM listOfAllRc $ \rc -> do
    rcNo <- decrypt rc.certificateNumber
    (totalEarning, distanceTravelled, totalRides, cancelledRides, duration) <- CQRide.fleetStatsByVehicle fleetOwnerId rcNo from to
    let totalDuration = calculateTimeDifference duration
    pure $
      Common.FleetEarningRes
        { driverId = Nothing,
          driverName = Nothing,
          totalRides = totalRides,
          totalEarning = totalEarning,
          vehicleNo = Just rcNo,
          status = Nothing,
          vehicleType = DCommon.castVehicleVariantDashboard rc.vehicleVariant,
          totalDuration = totalDuration,
          distanceTravelled = fromIntegral distanceTravelled / 1000.0,
          driverPhoneNo = Nothing,
          cancelledRides = cancelledRides
        }
  let summary = Common.Summary {totalCount = 10000, count = length res}
  pure $ Common.FleetEarningListRes {fleetEarningRes = res, summary}

calculateTimeDifference :: Int -> Common.TotalDuration
calculateTimeDifference diffTime = Common.TotalDuration {..}
  where
    diffTimeInSeconds :: Double
    diffTimeInSeconds = realToFrac diffTime

    hours :: Int
    hours = floor (diffTimeInSeconds / 3600)

    remainingSeconds :: Double
    remainingSeconds = diffTimeInSeconds - fromIntegral (hours * 3600)

    minutes :: Int
    minutes = floor (remainingSeconds / 60)

getListOfVehicles :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Common.FleetVehicleStatus -> Id DM.Merchant -> Maybe Text -> Maybe Text -> Flow [DVRC.VehicleRegistrationCertificate]
getListOfVehicles mbVehicleNo fleetOwnerId mbLimit mbOffset mbStatus merchantId mbSearchString statusAwareVehicleNo = do
  let limit = fromIntegral $ min 10 $ fromMaybe 5 mbLimit
      offset = fromIntegral $ fromMaybe 0 mbOffset
  case mbVehicleNo of
    Just vehicleNo -> RCQuery.partialFindLastVehicleRCFleet vehicleNo fleetOwnerId limit offset
    Nothing -> do
      case mbStatus of
        -- This Status is Associated with Driver and
        Just Common.Active -> RCQuery.findAllActiveRCForFleetByLimitOffset fleetOwnerId merchantId limit offset mbSearchString statusAwareVehicleNo
        Just Common.InActive -> RCQuery.findAllInactiveRCForFleet fleetOwnerId limit offset merchantId statusAwareVehicleNo
        -- This Status is only Associated purely with RCs and Not Associated with any Driver

        -- make changes here for onride and tripassigned
        Just Common.OnRide -> RCQuery.findAllVehicleByStatusForFleetByLimitOffset fleetOwnerId merchantId limit offset mbSearchString statusAwareVehicleNo DTT.IN_PROGRESS
        Just Common.TripAssigned -> RCQuery.findAllVehicleByStatusForFleetByLimitOffset fleetOwnerId merchantId limit offset mbSearchString statusAwareVehicleNo DTT.TRIP_ASSIGNED
        Just Common.Valid -> RCQuery.findAllRCByStatusForFleet fleetOwnerId (Just $ castFleetVehicleStatus mbStatus) limit offset merchantId statusAwareVehicleNo
        Just Common.Invalid -> RCQuery.findAllRCByStatusForFleet fleetOwnerId (Just $ castFleetVehicleStatus mbStatus) limit offset merchantId statusAwareVehicleNo
        Just Common.Pending -> RCQuery.findAllRCByStatusForFleet fleetOwnerId (Just $ castFleetVehicleStatus mbStatus) limit offset merchantId statusAwareVehicleNo
        Nothing -> RCQuery.findAllRCByStatusForFleet fleetOwnerId Nothing limit offset merchantId statusAwareVehicleNo

castFleetVehicleStatus :: Maybe Common.FleetVehicleStatus -> Documents.VerificationStatus
castFleetVehicleStatus = \case
  Just Common.Pending -> Documents.PENDING
  Just Common.Invalid -> Documents.INVALID
  _ -> Documents.VALID

---------------------------------------------------------------------
getDriverFleetDriverEarning ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Bool ->
  Maybe Common.SortOn ->
  Flow Common.FleetEarningListRes
getDriverFleetDriverEarning merchantShortId _ fleetOwnerId mbMobileCountryCode mbDriverPhNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn = do
  now <- getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  merchant <- findMerchantByShortId merchantShortId
  rideIds <- findIdsByFleetOwner (Just fleetOwnerId) from to
  driverId <- case mbDriverPhNo of
    Just driverPhNo -> do
      mobileNumberHash <- getDbHash driverPhNo
      let countryCode = fromMaybe (P.getCountryMobileCode merchant.country) mbMobileCountryCode
      driver <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
      fleetDriverAssociation <- FDV.findByDriverIdAndFleetOwnerId driver.id fleetOwnerId True
      when (isNothing fleetDriverAssociation) $ throwError (DriverNotLinkedToFleet driver.id.getId)
      pure $ Just driver.id
    Nothing -> pure Nothing
  driverStatsList <- CQRide.fleetStatsByDriver rideIds driverId from to mbLimit mbOffset mbSortDesc mbSortOn
  let driverStatsMap = Map.fromList [(driverStatsId, stats) | stats <- driverStatsList, Just driverStatsId <- [stats.driverId']]
      driverIds = Map.keys driverStatsMap

  -- Fetch driver information only for drivers in the stats list
  driverListWithInfo <- QPerson.findAllPersonAndDriverInfoWithDriverIds driverIds

  -- Process each driver with their corresponding stats
  res <- forM driverListWithInfo $ \(driver, driverInfo') -> do
    -- Common data preparation for all drivers
    let driverName = driver.firstName <> " " <> fromMaybe "" driver.lastName
    mobileNumber <- mapM decrypt driver.mobileNumber

    let statsData = Map.lookup (driver.id) driverStatsMap
    pure $
      Common.FleetEarningRes
        { driverId = Just $ cast @DP.Person @Common.Driver driver.id,
          driverName = Just driverName,
          status = Just $ castDriverStatus driverInfo'.mode,
          vehicleNo = Nothing,
          vehicleType = Nothing,
          driverPhoneNo = mobileNumber,
          totalRides = maybe 0 (.completedRides) statsData,
          totalEarning = maybe 0 (.totalEarnings) statsData,
          totalDuration = calculateTimeDifference $ maybe 0 (.totalDuration) statsData,
          distanceTravelled = maybe 0 (\stats -> fromIntegral stats.totalDistanceTravelled / 1000.0) statsData,
          cancelledRides = maybe 0 (.cancelledRides) statsData
        }
  let summary = Common.Summary {totalCount = 10000, count = length res}
  pure $ Common.FleetEarningListRes {fleetEarningRes = res, summary}

castDriverStatus :: Maybe DrInfo.DriverMode -> Common.DriverMode
castDriverStatus = \case
  Just DrInfo.ONLINE -> Common.ONLINE
  Just DrInfo.OFFLINE -> Common.OFFLINE
  Just DrInfo.SILENT -> Common.SILENT
  Nothing -> Common.OFFLINE

---------------------------------------------------------------------
getDriverFleetStatus :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Flow Common.DriverStatusRes
getDriverFleetStatus merchantShortId opCity requestorId mbFleetOwnerId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
  unless allowCacheDriverFlowStatus $ throwError (InvalidRequest "Cache driver flow status is not allowed in this merchant")
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  let allKeys = DDF.allKeys entityId
  logTagInfo "DriverStatus" $ "Checking Redis for keys: " <> show allKeys <> ", entityRole: " <> show entityRole <> ", entityId: " <> entityId
  redisCounts <-
    mapM
      ( \key ->
          Redis.get @Int key >>= \v -> do
            logTagInfo "DriverStatus" $ "Redis.get " <> key <> " => " <> show v
            pure v
      )
      allKeys
  if all isJust redisCounts
    then do
      logTagInfo "DriverStatus" $ "Cache hit for all statuses for entityId: " <> entityId <> ", counts: " <> show redisCounts
      pure $ SDF.toDriverStatusRes (zip (map Just DDF.statusList) (map (fromMaybe 0) redisCounts))
    else do
      logTagInfo "DriverStatus" $ "Cache miss for some statuses for entityId: " <> entityId <> ". Checking inProgress key. Redis counts: " <> show redisCounts
      SDF.handleCacheMissForDriverFlowStatus entityRole entityId allKeys

---------------------------------------------------------------------
getDriverFleetDriverVehicleAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverVehicleAssociation merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  (listOfAllDrivers, _, _) <- getListOfDrivers mbCountryCode mbPhoneNo fleetOwnerId merchant.id Nothing mbLimit mbOffset Nothing Nothing Nothing
  listOfAllVehicle <- getListOfVehicles mbVehicleNo fleetOwnerId mbLimit mbOffset Nothing merchant.id Nothing Nothing
  listItems <- createDriverVehicleAssociationListItem listOfAllDrivers listOfAllVehicle
  let filteredItems = filter (.isRcAssociated) listItems
  let summary = Common.Summary {totalCount = 10000, count = length filteredItems}
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = filteredItems, summary = summary}
  where
    createDriverVehicleAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => [FleetDriverAssociation] -> [DVRC.VehicleRegistrationCertificate] -> m [Common.DriveVehicleAssociationListItem]
    createDriverVehicleAssociationListItem fdaList vrcaList = do
      now <- getCurrentTime
      let defaultFrom = UTCTime (utctDay now) 0
          from = fromMaybe defaultFrom mbFrom
          to = fromMaybe now mbTo
      fmap concat $
        forM fdaList $ \fda -> do
          forM vrcaList $ \vrca -> do
            let driverId = fda.driverId
            driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            let driverName = Just driver.firstName
                conductorName = driver.lastName
            driverInfo' <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
            decryptedVehicleRC <- decrypt vrca.certificateNumber
            rcAssociation <- QRCAssociation.findLinkedByRCIdAndDriverId driverId vrca.id now
            let vehicleType = DCommon.castVehicleVariantDashboard vrca.vehicleVariant
            (completedRides, earning) <- case mbStatus of
              Just True -> do
                rides <- CQRide.totalRidesByFleetOwnerPerVehicleAndDriver (Just fleetOwnerId) decryptedVehicleRC driverId from to
                earnings <- CQRide.totalEarningsByFleetOwnerPerVehicleAndDriver (Just fleetOwnerId) decryptedVehicleRC driverId from to
                pure (rides, earnings)
              _ -> pure (0, 0)
            let isDriverActive = fda.isActive
            let isRcAssociated = isJust rcAssociation
            driverPhoneNo <- mapM decrypt driver.mobileNumber
            let listItem =
                  Common.DriveVehicleAssociationListItem
                    { vehicleNo = Just decryptedVehicleRC,
                      status = Just $ castDriverStatus driverInfo'.mode,
                      driverId = Just driverId.getId,
                      verificationDocsStatus = Nothing,
                      isDriverOnRide = Nothing,
                      isDriverOnPickup = Nothing,
                      upcomingRouteCode = Nothing,
                      ..
                    }
            pure listItem

getListOfDrivers :: Maybe Text -> Maybe Text -> Text -> Id DM.Merchant -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Common.DriverMode -> Maybe Text -> Maybe Text -> Flow ([FleetDriverAssociation], [DP.Person], [DI.DriverInformation])
getListOfDrivers _ mbDriverPhNo fleetOwnerId _ mbIsActive mbLimit mbOffset mbMode mbName mbSearchString = do
  let limit = min 10 $ fromMaybe 5 mbLimit
      offset = fromMaybe 0 mbOffset

  mobileNumberHash <- case mbSearchString of
    Just _ -> pure Nothing
    Nothing -> case mbDriverPhNo of
      Just phNo -> Just <$> getDbHash phNo
      Nothing -> pure Nothing

  let mode = castDashboardDriverStatus <$> mbMode
  driverAssociationAndInfo <- FDV.findAllActiveDriverByFleetOwnerIdWithDriverInfo fleetOwnerId limit offset mobileNumberHash mbName mbSearchString mbIsActive mode
  let (fleetDriverAssociation, person, driverInformation) = unzip3 driverAssociationAndInfo
  return (fleetDriverAssociation, person, driverInformation)

castDashboardDriverStatus :: Common.DriverMode -> DrInfo.DriverMode
castDashboardDriverStatus = \case
  Common.ONLINE -> DrInfo.ONLINE
  Common.OFFLINE -> DrInfo.OFFLINE
  Common.SILENT -> DrInfo.SILENT

---------------------------------------------------------------------

getDriverFleetDriverAssociation :: ShortId DM.Merchant -> Context.City -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Flow Common.DrivertoVehicleAssociationResT
getDriverFleetDriverAssociation merchantShortId _opCity mbIsActive mbLimit mbOffset mbCountryCode mbDriverPhNo mbStats mbFrom mbTo mbMode mbName mbSearchString mbFleetOwnerId mbRequestorId hasFleetMemberHierarchy isRequestorFleerOwner mbHasRequestReason = do
  requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  fleetOwnersInfo <- getFleetOwnersInfoMerchantBased mbFleetOwnerId mbRequestorId hasFleetMemberHierarchy isRequestorFleerOwner
  when (hasFleetMemberHierarchy == Just False) $ mapM_ (checkRequestorAccessToFleet mbRequestorId . (.fleetOwnerId)) fleetOwnersInfo
  merchant <- findMerchantByShortId merchantShortId
  when (fromMaybe False merchant.fleetOwnerEnabledCheck) $ mapM_ (\info -> DCommon.checkFleetOwnerVerification info.fleetOwnerId merchant.fleetOwnerEnabledCheck) fleetOwnersInfo
  let fleetOwnerIds = map (.fleetOwnerId) fleetOwnersInfo
      fleetOwnerNameMap = Map.fromList $ map (\info -> (info.fleetOwnerId, info.fleetOwnerName)) fleetOwnersInfo
  listOfAllDrivers <- getListOfDriversMultiFleet mbCountryCode mbDriverPhNo fleetOwnerIds merchant.id mbIsActive mbLimit mbOffset mbMode mbName mbSearchString mbHasRequestReason
  listItems <- createFleetDriverAssociationListItem fleetOwnerNameMap listOfAllDrivers
  let summary = Common.Summary {totalCount = 10000, count = length listItems}
  pure $
    Common.DrivertoVehicleAssociationResT
      { fleetOwnerId = requestorId, -- Kept for Backward Compatibility, Don't use this Fleet Owner Id for Multiple Fleets
        listItem = listItems,
        summary = summary
      }
  where
    createFleetDriverAssociationListItem :: Map.Map Text Text -> ([FleetDriverAssociation], [DP.Person], [DI.DriverInformation]) -> Flow [Common.DriveVehicleAssociationListItemT]
    createFleetDriverAssociationListItem fleetOwnerNameMap (fdaList, personList, driverInfoList) = do
      let driverListWithInfo = zip personList driverInfoList
      now <- getCurrentTime
      let defaultFrom = UTCTime (utctDay now) 0
          from = fromMaybe defaultFrom mbFrom
          to = fromMaybe now mbTo
      forM (zip driverListWithInfo fdaList) $ \((driver, driverInfo'), fda) -> do
        let fleetOwnerId = fda.fleetOwnerId
        let fleetOwnerName = fromMaybe "" (Map.lookup fleetOwnerId fleetOwnerNameMap)
        driverRCAssociation <- QRCAssociation.findAllActiveAndInactiveAssociationsByDriverId driver.id
        let rcAssociatedWithFleet = filter (\(_, rc) -> rc.fleetOwnerId == Just fleetOwnerId) driverRCAssociation
        (vehicleNo, vehicleType) <- case rcAssociatedWithFleet of ---- so the logic is if it have active association with the fleet vehicle return that otherwise return the latest one
          [] -> pure (Nothing, Nothing)
          associations -> do
            let activeAssociation = find (\(assoc, _) -> assoc.isRcActive) associations
            case activeAssociation of
              Just (_, rc) -> getVehicleDetails rc ------- if driver is using fleet vehicle
              Nothing -> getVehicleDetails $ snd $ head associations -------- otherwise give the latest active association
        let driverName = Just driver.firstName
            conductorName = driver.lastName
        driverPhoneNo <- mapM decrypt driver.mobileNumber
        driverLicenseStatus <- do
          mbDl <- B.runInReplica $ QDriverLicense.findByDriverId driver.id
          case mbDl of
            Just dl -> do
              let dlStatus = DCommon.castVerificationStatus dl.verificationStatus
              pure dlStatus
            Nothing -> pure Common.PENDING
        panCardStatus <- do
          mbPan <- B.runInReplica $ QPanCard.findByDriverId driver.id
          case mbPan of
            Just pan -> do
              let panStatus = DCommon.castVerificationStatus pan.verificationStatus
              pure panStatus
            Nothing -> pure Common.PENDING
        aadhaarStatus <- do
          mbAadhaar <- B.runInReplica $ QAadhaarCard.findByPrimaryKey driver.id
          case mbAadhaar of
            Just aadhaar -> do
              let aadhaarStatus = DCommon.castVerificationStatus aadhaar.verificationStatus
              pure aadhaarStatus
            Nothing -> pure Common.PENDING
        (completedRides, earning) <- case mbStats of
          Just True -> do
            rides <- CQRide.totalRidesByFleetOwnerPerDriver (Just fleetOwnerId) driver.id from to
            earnings <- CQRide.totalEarningsByFleetOwnerPerDriver (Just fleetOwnerId) driver.id from to
            pure (rides, earnings)
          _ -> pure (0, 0)
        let driverStatus = Just $ castDriverStatus driverInfo'.mode -- if isNothing vehicleNo then Nothing else Just $ castDriverStatus driverInfo'.mode
        (isDriverOnPickup, isDriverOnRide, routeCode) <-
          if driverInfo'.onRide
            then do
              currentTripTransaction <- WMB.findNextActiveTripTransaction fleetOwnerId driver.id
              case currentTripTransaction of
                Just tripTransation -> return (tripTransation.status == TRIP_ASSIGNED, tripTransation.status == IN_PROGRESS, Just tripTransation.routeCode)
                Nothing -> return (False, False, Nothing)
            else return (False, False, Nothing)
        let isRcAssociated = isJust vehicleNo
            isDriverActive = fda.isActive
            driverId = Just $ driver.id.getId
            requestReason = fda.requestReason
            responseReason = fda.responseReason
        let ls =
              Common.DriveVehicleAssociationListItemT
                { vehicleNo = vehicleNo,
                  status = driverStatus,
                  isDriverActive = isDriverActive,
                  isDriverOnRide = Just isDriverOnRide,
                  isDriverOnPickup = Just isDriverOnPickup,
                  associatedOn = fda.associatedOn,
                  upcomingRouteCode = routeCode,
                  verificationDocsStatus =
                    Just
                      Common.VerificationDocsStatus
                        { driverLicense = Just driverLicenseStatus,
                          panCard = Just panCardStatus,
                          aadhaarCard = Just aadhaarStatus,
                          vehicleRegistrationCertificate = Nothing,
                          vehicleFitness = Nothing,
                          vehiclePermit = Nothing,
                          vehiclePUC = Nothing,
                          vehicleInsurance = Nothing,
                          vehicleLeft = Nothing,
                          vehicleRight = Nothing,
                          vehicleFront = Nothing,
                          vehicleBack = Nothing,
                          vehicleFrontInterior = Nothing,
                          vehicleBackInterior = Nothing,
                          odometer = Nothing
                        },
                  ..
                }
        pure ls
    getVehicleDetails :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DVRC.VehicleRegistrationCertificate -> m (Maybe Text, Maybe Common.VehicleVariant)
    getVehicleDetails vrc = do
      decryptedVehicleRC <- decrypt vrc.certificateNumber
      let vehicleType = DCommon.castVehicleVariantDashboard vrc.vehicleVariant
      pure (Just decryptedVehicleRC, vehicleType)

---------------------------------------------------------------------
getDriverFleetVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Flow Common.DrivertoVehicleAssociationResT
getDriverFleetVehicleAssociation merchantShortId _opCity mbLimit mbOffset mbVehicleNumber mbIncludeStats mbFrom mbTo mbStatus mbSearchString mbStatusAwareVehicleNo mbFleetOwnerId mbRequestorId hasFleetMemberHierarchy isRequestorFleerOwner = do
  requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  fleetOwnersInfo <- getFleetOwnersInfoMerchantBased mbFleetOwnerId mbRequestorId hasFleetMemberHierarchy isRequestorFleerOwner
  when (hasFleetMemberHierarchy == Just False) $ mapM_ (checkRequestorAccessToFleet mbRequestorId . (.fleetOwnerId)) fleetOwnersInfo
  merchant <- findMerchantByShortId merchantShortId
  when (fromMaybe False merchant.fleetOwnerEnabledCheck) $ mapM_ (\info -> DCommon.checkFleetOwnerVerification info.fleetOwnerId merchant.fleetOwnerEnabledCheck) fleetOwnersInfo
  let fleetOwnerIds = map (.fleetOwnerId) fleetOwnersInfo
      fleetOwnerNameMap = Map.fromList $ map (\info -> (info.fleetOwnerId, info.fleetOwnerName)) fleetOwnersInfo
  listOfAllVehicle <- getListOfVehiclesMultiFleet mbVehicleNumber fleetOwnerIds mbLimit mbOffset mbStatus merchant.id mbSearchString mbStatusAwareVehicleNo
  let listOfAllVehicleWithFleetInfo = catMaybes $ map (\vrc -> vrc.fleetOwnerId >>= \fleetOwnerId -> Just (vrc, fleetOwnerId, fromMaybe "" (Map.lookup fleetOwnerId fleetOwnerNameMap))) listOfAllVehicle
  listItems <- createFleetVehicleAssociationListItem listOfAllVehicleWithFleetInfo
  let summary = Common.Summary {totalCount = 10000, count = length listItems}
  pure $
    Common.DrivertoVehicleAssociationResT
      { fleetOwnerId = requestorId, -- Kept for Backward Compatibility, Don't use this Fleet Owner Id for Multiple Fleets
        listItem = listItems,
        summary = summary
      }
  where
    createFleetVehicleAssociationListItem :: [(DVRC.VehicleRegistrationCertificate, Text, Text)] -> Flow [Common.DriveVehicleAssociationListItemT]
    createFleetVehicleAssociationListItem vrcListWithFleetInfo = do
      now <- getCurrentTime
      forM vrcListWithFleetInfo $ \(vrc, fleetOwnerId, fleetOwnerName) -> do
        decryptedVehicleRC <- decrypt vrc.certificateNumber
        let defaultFrom = UTCTime (utctDay now) 0
            from = fromMaybe defaultFrom mbFrom
            to = fromMaybe now mbTo
        stats <- case mbIncludeStats of
          Just _ -> do
            completedRides <- CQRide.totalRidesByFleetOwnerPerVehicle (Just fleetOwnerId) decryptedVehicleRC from to
            earning <- CQRide.totalEarningsByFleetOwnerPerVehicle (Just fleetOwnerId) decryptedVehicleRC from to
            return (completedRides, earning)
          Nothing -> return (0, 0) ------------ when we are not including stats then we will return 0
        rcActiveAssociation <- QRCAssociation.findActiveAssociationByRC vrc.id True
        ((driverName, conductorName, driverId, driverPhoneNo, driverStatus, isDriverOnPickup, isDriverOnRide, routeCode), mbAssociatedOn) <- case rcActiveAssociation of
          Just activeAssociation -> do
            driverInfo <- getFleetDriverInfo fleetOwnerId activeAssociation.driverId False
            return (driverInfo, Just activeAssociation.associatedOn)
          ------- when vehicle is in active state
          Nothing -> do
            latestAssociation <- QRCAssociation.findLatestLinkedByRCId vrc.id now ------- when there is not any active association then i will find out the latest association  (vehicle is in inActive state)
            case latestAssociation of
              Just latestAssoc -> do
                driverInfo <- getFleetDriverInfo fleetOwnerId latestAssoc.driverId False
                return (driverInfo, Just latestAssoc.associatedOn)
              Nothing -> pure ((Nothing, Nothing, Nothing, Nothing, Nothing, Just False, Just False, Nothing), Nothing) -------- when vehicle is unAssigned
        let vehicleType = DCommon.castVehicleVariantDashboard vrc.vehicleVariant
        let isDriverActive = isJust driverName -- Check if there is a current active driver
        let isRcAssociated = isJust rcActiveAssociation
        vehicleImages <- QImage.findAllByRcId (Just vrc.id.getId)
        let verificationDocs =
              Common.VerificationDocsStatus
                { vehicleRegistrationCertificate = Just $ DCommon.castVerificationStatus vrc.verificationStatus,
                  vehiclePermit = Just $ if any (\img -> img.imageType == DDoc.VehiclePermit && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING, ------ currently we are not verifying these docs therefore
                  vehicleInsurance = Just $ if any (\img -> img.imageType == DDoc.VehicleInsurance && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleFitness = Just $ if any (\img -> img.imageType == DDoc.VehicleFitnessCertificate && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehiclePUC = Just $ if any (\img -> img.imageType == DDoc.VehiclePUC && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleFront = Just $ if any (\img -> img.imageType == DDoc.VehicleFront && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleBack = Just $ if any (\img -> img.imageType == DDoc.VehicleBack && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleFrontInterior = Just $ if any (\img -> img.imageType == DDoc.VehicleFrontInterior && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleBackInterior = Just $ if any (\img -> img.imageType == DDoc.VehicleBackInterior && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleLeft = Just $ if any (\img -> img.imageType == DDoc.VehicleLeft && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  vehicleRight = Just $ if any (\img -> img.imageType == DDoc.VehicleRight && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  odometer = Just $ if any (\img -> img.imageType == DDoc.Odometer && img.verificationStatus == Just Documents.VALID) vehicleImages then Common.VALID else Common.PENDING,
                  driverLicense = Nothing,
                  panCard = Nothing,
                  aadhaarCard = Nothing
                }
        let ls =
              Common.DriveVehicleAssociationListItemT
                { vehicleNo = Just decryptedVehicleRC,
                  status = Just $ castDriverStatus driverStatus,
                  isDriverOnRide = isDriverOnRide,
                  isDriverOnPickup = isDriverOnPickup,
                  isDriverActive = isDriverActive,
                  earning = snd stats,
                  completedRides = fst stats,
                  vehicleType = vehicleType,
                  verificationDocsStatus = Just verificationDocs,
                  upcomingRouteCode = routeCode,
                  requestReason = Nothing,
                  responseReason = Nothing,
                  associatedOn = mbAssociatedOn,
                  ..
                }
        pure ls

---------------------------------------------------------------------
getDriverFleetDriverListStats ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Day ->
  Maybe Day ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Common.FleetDriverListStatsSortOn ->
  Maybe Common.FleetDriverStatsResponseType ->
  Flow Common.FleetDriverStatsListRes
getDriverFleetDriverListStats merchantShortId opCity fleetOwnerId mbFrom mbTo mbSearch mbLimit mbOffset sortDesc sortOnField mbResponseType = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  let useDBForAnalytics = transporterConfig.analyticsConfig.useDbForEarningAndMetrics

  now <- getCurrentTime
  let fromDay = fromMaybe (utctDay now) mbFrom
      toDay = fromMaybe (utctDay now) mbTo
      limit = max 0 $ min 10 . fromMaybe 10 $ mbLimit
      offset = max 0 $ fromMaybe 0 mbOffset
      responseType = fromMaybe Common.METRICS_LIST mbResponseType

  -- Fetch driver ids from ClickHouse association
  driverIdObjs <-
    if useDBForAnalytics
      then QFDAExtra.getActiveDriverIdsByFleetOwnerId fleetOwnerId
      else do
        maybeDriverIds <- CFDA.getDriverIdsByFleetOwnerId fleetOwnerId
        pure $ fromMaybe [] maybeDriverIds
  let driverIdTexts = map (.getId) driverIdObjs
      mbSearchTerm = do
        raw <- mbSearch
        let trimmed = T.strip raw
        guard (not $ T.null trimmed)
        pure trimmed

  (filteredDriverIds, preloadedMap) <-
    case mbSearchTerm of
      Nothing -> pure (driverIdTexts, Map.empty)
      Just searchTerm -> do
        mbMobileHash <- hashIfMobile searchTerm
        persons <- fetchPersons driverIdObjs (Just searchTerm) mbMobileHash useDBForAnalytics
        let driverIdsFiltered = map (.personId) persons
            personMap = Map.fromList $ map (\person -> (person.personId, buildDriverFullName person)) persons
        pure (driverIdsFiltered, personMap)

  driverStats <-
    case responseType of
      Common.EARNINGS_LIST -> do
        earningsStats <-
          if useDBForAnalytics
            then QFODSExtra.sumDriverEarningsByFleetOwnerIdAndDriverIdsDB fleetOwnerId filteredDriverIds fromDay toDay limit offset sortDesc sortOnField
            else CFODS.sumDriverEarningsByFleetOwnerIdAndDriverIds fleetOwnerId filteredDriverIds fromDay toDay limit offset sortDesc sortOnField
        nameMap <- ensureNameMap preloadedMap (map (.driverId) earningsStats) useDBForAnalytics
        let statsList = map (buildEarningsResponse transporterConfig) earningsStats
        forM statsList $ \(driverId, statRes) -> do
          let driverName = fromMaybe "Unknown" $ Map.lookup driverId nameMap
          pure $ Common.EarningsList $ statRes {Common.driverName = driverName}
      Common.METRICS_LIST -> do
        metricsStats <-
          if useDBForAnalytics
            then QFODSExtra.sumDriverMetricsByFleetOwnerIdAndDriverIdsDB fleetOwnerId filteredDriverIds fromDay toDay limit offset sortDesc sortOnField
            else CFODS.sumDriverMetricsByFleetOwnerIdAndDriverIds fleetOwnerId filteredDriverIds fromDay toDay limit offset sortDesc sortOnField
        nameMap <- ensureNameMap preloadedMap (map (.driverId) metricsStats) useDBForAnalytics
        let statsList = map (buildMetricsResponse transporterConfig fromDay toDay) metricsStats
        forM statsList $ \(driverId, statRes) -> do
          let driverName = fromMaybe "Unknown" $ Map.lookup driverId nameMap
          pure $ Common.MetricsList $ statRes {Common.driverName = driverName}

  let count = length driverStats
      summary = Common.Summary {totalCount = length filteredDriverIds, count}

  pure $ Common.FleetDriverStatsListRes {driverStats, summary}
  where
    hashIfMobile :: Text -> Flow (Maybe DbHash)
    hashIfMobile txt =
      if T.all isDigit txt
        then Just <$> getDbHash txt
        else pure Nothing

    fetchPersons :: [Id DP.Person] -> Maybe Text -> Maybe DbHash -> Bool -> Flow [CHPerson.PersonBasic]
    fetchPersons ids mbNameFilter mbMobileHash useDBForAnalytics =
      if useDBForAnalytics
        then do
          dbPersons <- QPersonExtra.findPersonsByIdsForAnalytics ids mbNameFilter mbMobileHash
          pure $ map personToBasic dbPersons
        else CHPerson.findPersonsByIds ids mbNameFilter mbMobileHash

    ensureNameMap :: Map.Map Text Text -> [Text] -> Bool -> Flow (Map.Map Text Text)
    ensureNameMap existingMap driverIds useDBForAnalytics
      | not (Map.null existingMap) || null driverIds = pure existingMap
      | otherwise = do
        persons <- fetchPersons (map Id driverIds) Nothing Nothing useDBForAnalytics
        pure $ Map.fromList $ map (\person -> (person.personId, buildDriverFullName person)) persons

    buildEarningsResponse :: DTCConfig.TransporterConfig -> QFODSExtra.DriverEarningsAggregated -> (Text, Common.FleetDriverEarningsStatsRes)
    buildEarningsResponse config agg =
      let onlineEarningGross = agg.onlineTotalEarningSum
          cashEarningGross = agg.cashTotalEarningSum
          totalEarningGross = liftA2 (+) onlineEarningGross cashEarningGross
          cashPlatformFees = agg.cashPlatformFeesSum
          onlinePlatformFees = agg.onlinePlatformFeesSum
          platformFeeTotal = liftA2 (+) cashPlatformFees onlinePlatformFees
          totalEarningNet = liftA2 (-) totalEarningGross platformFeeTotal
          inAppEarningGross = onlineEarningGross
          inAppEarningNet = liftA2 (-) inAppEarningGross onlinePlatformFees
          cashEarningNet = liftA2 (-) cashEarningGross cashPlatformFees
       in ( agg.driverId,
            Common.FleetDriverEarningsStatsRes
              { driverName = "", -- Will be set later
                totalEarningGross = totalEarningGross,
                inAppEarningGross = inAppEarningGross,
                cashEarningGross = cashEarningGross,
                platformFeeTotal = platformFeeTotal,
                totalEarningNet = totalEarningNet,
                inAppEarningNet = inAppEarningNet,
                cashEarningNet = cashEarningNet,
                currency = config.currency
              }
          )

    buildMetricsResponse :: DTCConfig.TransporterConfig -> Day -> Day -> QFODSExtra.DriverMetricsAggregated -> (Text, Common.FleetDriverMetricsStatsRes)
    buildMetricsResponse config _ _ agg =
      -- commenting for compilation
      let onlineEarning = agg.onlineTotalEarningSum
          cashEarning = agg.cashTotalEarningSum
          totalEarning = liftA2 (+) onlineEarning cashEarning
          completed = agg.totalCompletedRidesSum
          accepted = agg.acceptationRequestCountSum
          rejected = agg.rejectedRequestCountSum
          passed = agg.pulledRequestCountSum
          driverCanceled = agg.driverCancellationCountSum
          customerCanceled = agg.customerCancellationCountSum
          distance = agg.totalDistanceSum
          onlineDuration = agg.onlineDurationSum
          rideDuration = agg.rideDurationSum
          totalRatingScore = agg.totalRatingScoreSum
          totalRatingCount = agg.totalRatingCountSum
          totalRequests =
            case (accepted, rejected, passed) of
              (Nothing, Nothing, Nothing) -> Nothing
              _ ->
                Just $
                  fromMaybe 0 accepted
                    + fromMaybe 0 rejected
                    + fromMaybe 0 passed
          acceptanceRate =
            totalRequests >>= \tr ->
              if tr > 0
                then accepted >>= \a -> Just (fromIntegral a / fromIntegral tr * 100)
                else Nothing
          completionRate =
            totalRequests >>= \tr ->
              if tr > 0
                then completed >>= \c -> Just (fromIntegral c / fromIntegral tr * 100)
                else Nothing
          onlineDurationInSeconds = onlineDuration <&> getSeconds
          rideDurationInSeconds = rideDuration <&> getSeconds
          utilization =
            rideDurationInSeconds >>= \rd ->
              onlineDurationInSeconds >>= \od ->
                if od > 0 then Just (fromIntegral rd / fromIntegral od * 100) else Nothing
          distanceKm =
            distance <&> \d -> fromIntegral (getMeters d) / 1000.0
          earningPerKm =
            distanceKm >>= \dkm ->
              if dkm > 0
                then totalEarning >>= \te -> Just (te / HighPrecMoney dkm)
                else Nothing
          -- Rating: totalRatingScore / completedRides
          rating =
            totalRatingCount >>= \trc ->
              totalRatingScore >>= \trs ->
                if trc > 0 && trs > 0
                  then Just (fromIntegral trs / fromIntegral trc)
                  else Nothing
       in ( agg.driverId,
            Common.FleetDriverMetricsStatsRes
              { driverName = "", -- Will be set later
                rating = rating,
                acceptedRideRequests = accepted,
                rejectedRideRequests = rejected,
                passedRideRequests = passed,
                acceptanceRate = acceptanceRate,
                completedRides = completed,
                driverCanceledRides = driverCanceled,
                customerCanceledRides = customerCanceled,
                completionRate = completionRate,
                onlineDuration = onlineDuration,
                rideDuration = rideDuration,
                utilization = utilization,
                distance = distance,
                earnings = fmap (`PriceAPIEntity` config.currency) totalEarning,
                earningPerKm = fmap (`PriceAPIEntity` config.currency) earningPerKm
              }
          )

    buildDriverFullName :: CHPerson.PersonBasic -> Text
    buildDriverFullName person =
      let nameParts = mapMaybe (fmap T.strip) [Just person.firstNameValue, person.middleNameValue, person.lastNameValue]
          nameText = T.unwords $ filter (not . T.null) nameParts
       in if T.null nameText then "Unknown" else nameText

    personToBasic :: DP.Person -> CHPerson.PersonBasic
    personToBasic person =
      CHPerson.PersonBasic
        { personId = person.id.getId,
          firstNameValue = person.firstName,
          middleNameValue = person.middleName,
          lastNameValue = person.lastName
        }

---------------------------------------------------------------------

getFleetDriverInfo :: Text -> Id DP.Person -> Bool -> Flow (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe DrInfo.DriverMode, Maybe Bool, Maybe Bool, Maybe Text)
getFleetDriverInfo fleetOwnerId driverId isDriver = do
  mbDriver <- QPerson.findById driverId
  mbDriverInfo' <- QDriverInfo.findById driverId
  case (mbDriverInfo', mbDriver) of
    (Just driverInfo', Just driver) -> do
      currentTripTransaction <- WMB.findNextActiveTripTransaction fleetOwnerId driver.id
      mode <-
        if isDriver
          then return (driverInfo'.mode)
          else return Nothing
      (isDriverOnPickup, isDriverOnRide, routeCode) <-
        if driverInfo'.onRide
          then do
            case currentTripTransaction of
              Just tripTransation -> return (tripTransation.status == TRIP_ASSIGNED, tripTransation.status == IN_PROGRESS, Just tripTransation.routeCode)
              Nothing -> return (False, False, Nothing)
          else return (False, False, Nothing)
      mobileNumber <- mapM decrypt driver.mobileNumber
      return (Just driver.firstName, driver.lastName, Just driver.id.getId, mobileNumber, mode, Just isDriverOnPickup, Just isDriverOnRide, routeCode)
    (_, _) -> pure (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

---------------------------------------------------------------------
postDriverFleetVehicleDriverRcStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Maybe Text ->
  Common.RCStatusReq ->
  Flow APISuccess
postDriverFleetVehicleDriverRcStatus merchantShortId opCity reqDriverId requestorId mbFleetOwnerId req = do
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  case entityRole of
    DP.FLEET_OWNER -> do
      DCommon.checkFleetOwnerVerification entityId merchant.fleetOwnerEnabledCheck
      validateFleetOwnerWithDriverAndVehicle personId entityId merchant.id merchantOpCityId req.rcNo
    DP.OPERATOR -> validateOperatorWithDriver personId entityId
    _ -> throwError (InvalidRequest "Invalid Data")
  _ <- DomainRC.linkRCStatus (personId, merchant.id, merchantOpCityId) (DomainRC.RCStatusReq {isActivate = req.isActivate, rcNo = req.rcNo})
  logTagInfo "dashboard -> addVehicle : " (show personId)
  pure Success
  where
    validateFleetOwnerWithDriverAndVehicle :: Id DP.Person -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Flow ()
    validateFleetOwnerWithDriverAndVehicle personId fleetOwnerId merchantId merchantOpCityId rcNo = do
      driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId personId fleetOwnerId True
      when (isNothing isFleetDriver) $ throwError DriverNotPartOfFleet
      unless (merchantId == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $
        throwError (PersonDoesNotExist personId.getId)
      vehicle <- RCQuery.findLastVehicleRCWrapper rcNo >>= fromMaybeM (VehicleDoesNotExist rcNo)
      unless (isJust vehicle.fleetOwnerId && vehicle.fleetOwnerId == Just fleetOwnerId) $
        throwError (FleetOwnerVehicleMismatchError fleetOwnerId)

    validateOperatorWithDriver :: Id DP.Person -> Text -> Flow ()
    validateOperatorWithDriver personId operatorId = do
      isDriverOperator <- DDriver.checkDriverOperatorAssociation personId (Id operatorId)
      when (not isDriverOperator) $ throwError DriverNotPartOfOperator

---------------------------------------------------------------------
postDriverUpdateFleetOwnerInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.UpdateFleetOwnerInfoReq ->
  Flow APISuccess
postDriverUpdateFleetOwnerInfo merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  runRequestValidation (Common.validateUpdateFleetOwnerInfoReq merchantOpCity.country) req
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  whenJust req.mobileNo $ \reqMobileNo -> do
    mobileNumberHash <- getDbHash reqMobileNo
    let countryCodeFallback = P.getCountryMobileCode merchantOpCity.country
    person <- QPerson.findByMobileNumberAndMerchantAndRole (fromMaybe countryCodeFallback req.mobileCountryCode) mobileNumberHash merchant.id DP.FLEET_OWNER
    when (isJust person) $ throwError (MobileNumberAlreadyLinked reqMobileNo)
  whenJust req.email $ \reqEmail -> do
    person <- QPerson.findByEmailAndMerchantIdAndRole (Just reqEmail) merchant.id DP.FLEET_OWNER
    when (isJust person) $ throwError (EmailAlreadyLinked reqEmail)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  encNewPhoneNumber <- forM req.mobileNo encrypt
  let updDriver =
        driver
          { DP.mobileCountryCode = req.mobileCountryCode,
            DP.mobileNumber = encNewPhoneNumber,
            DP.email = req.email,
            DP.firstName = fromMaybe driver.firstName req.firstName,
            DP.lastName = req.lastName
          }
  mbUpdFleetOwnerinfo <- do
    fleetOwnerInfo <- B.runInReplica (FOI.findByPrimaryKey personId) >>= fromMaybeM (InvalidRequest "Fleet owner information does not exist")
    reqStripeIdNumber <- forM req.stripeIdNumber encrypt
    let updFleetOwnerInfo =
          fleetOwnerInfo
            { DFOI.stripeIdNumber = reqStripeIdNumber <|> fleetOwnerInfo.stripeIdNumber,
              DFOI.stripeAddress = req.stripeAddress <|> fleetOwnerInfo.stripeAddress,
              DFOI.fleetDob = req.fleetDob <|> fleetOwnerInfo.fleetDob,
              DFOI.fleetName = req.fleetName <|> fleetOwnerInfo.fleetName,
              DFOI.fleetType = fromMaybe fleetOwnerInfo.fleetType (DRegV2.castFleetType <$> req.fleetType)
            }
    pure $ Just updFleetOwnerInfo

  QPerson.updateFleetOwnerDetails personId updDriver
  whenJust mbUpdFleetOwnerinfo FOI.updateFleetOwnerInfo
  pure Success

---------------------------------------------------------------------
getDriverFleetOperatorInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Flow Common.FleetOwnerInfoRes
getDriverFleetOperatorInfo merchantShortId opCity personId = do
  getDriverFleetOwnerInfo merchantShortId opCity (Id personId)

getDriverFleetOwnerInfo :: -- Deprecated, use getDriverFleetOperatorInfo
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo requestorMerchantShortId requestorCity driverId = do
  let personId = cast @Common.Driver @DP.Person driverId
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantOpCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  unless (merchantOpCity.merchantShortId == requestorMerchantShortId && merchantOpCity.city == requestorCity) $ throwError (PersonDoesNotExist personId.getId)
  mbFleetOwnerInfo <- B.runInReplica $ FOI.findByPrimaryKey personId
  case mbFleetOwnerInfo of
    Nothing -> do
      unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Person is not a fleet owner or operator")
      referral <- QDR.findById personId
      contact <- mapM decrypt person.mobileNumber
      let name = person.firstName <> maybe "" (" " <>) person.middleName <> maybe "" (" " <>) person.lastName
      pure
        Common.FleetOwnerInfoRes
          { id = person.id.getId,
            mobileNo = contact,
            name = Just name,
            fleetType = "",
            referralCode = (.referralCode.getId) <$> referral,
            blocked = False,
            enabled = True,
            verified = True,
            gstNumber = Nothing,
            gstImageId = Nothing,
            panNumber = Nothing,
            aadhaarNumber = Nothing,
            fleetConfig = Nothing,
            operatorName = Nothing,
            operatorContact = Nothing,
            registeredAt = Nothing,
            businessLicenseNumber = Nothing,
            approvedBy = Nothing,
            roleName = Just (show person.role),
            referredByOperatorId = Nothing,
            isEligibleForSubscription = Nothing,
            fleetDob = Nothing,
            stripeAddress = Nothing,
            stripeIdNumber = Nothing,
            updatedAt = person.updatedAt
          }
    Just fleetOwnerInfo -> do
      fleetConfig <- QFC.findByPrimaryKey personId
      mbFleetOperatorAssoc <- FOV.findByFleetOwnerId personId.getId True
      (operatorName, operatorContact) <- case mbFleetOperatorAssoc of
        Nothing -> pure (Nothing, Nothing)
        Just fleetOperatorAssoc -> do
          operator <- QPerson.findById (Id fleetOperatorAssoc.operatorId) >>= fromMaybeM (PersonDoesNotExist fleetOperatorAssoc.operatorId)
          contact <- mapM decrypt operator.mobileNumber
          pure $ (Just (operator.firstName <> fromMaybe "" operator.middleName <> fromMaybe "" operator.lastName), contact)
      makeFleetOwnerInfoRes fleetConfig fleetOwnerInfo person operatorName operatorContact
  where
    makeFleetOwnerInfoRes :: Maybe DFC.FleetConfig -> DFOI.FleetOwnerInformation -> DP.Person -> Maybe Text -> Maybe Text -> Flow Common.FleetOwnerInfoRes
    makeFleetOwnerInfoRes mbFleetConfig DFOI.FleetOwnerInformation {..} fleetOwner operatorName operatorContact = do
      referral <- QDR.findById fleetOwnerPersonId
      let fleetConfig =
            mbFleetConfig <&> \fleetConfig' ->
              Common.FleetConfig
                { allowAutomaticRoundTripAssignment = fleetConfig'.allowAutomaticRoundTripAssignment,
                  allowEndingMidRoute = fleetConfig'.allowEndingMidRoute,
                  allowStartRideFromQR = fleetConfig'.allowStartRideFromQR,
                  endRideDistanceThreshold = fleetConfig'.endRideDistanceThreshold,
                  rideEndApproval = fleetConfig'.rideEndApproval
                }
      gstNumber' <- decryptWithDefault gstNumber gstNumberDec
      panNumber' <- decryptWithDefault panNumber panNumberDec
      aadhaarNumber' <- decryptWithDefault aadhaarNumber aadhaarNumberDec
      businessLicenseNumber' <- decryptWithDefault businessLicenseNumber businessLicenseNumberDec
      stripeIdNumber' <- forM stripeIdNumber decrypt
      let name = fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.middleName <> maybe "" (" " <>) fleetOwner.lastName
      mobileNo' <- mapM decrypt fleetOwner.mobileNumber
      return $
        Common.FleetOwnerInfoRes
          { fleetType = show fleetType,
            referralCode = (.referralCode.getId) <$> referral,
            gstNumber = gstNumber',
            panNumber = panNumber',
            aadhaarNumber = aadhaarNumber',
            businessLicenseNumber = businessLicenseNumber',
            stripeIdNumber = stripeIdNumber',
            approvedBy = Nothing,
            id = fleetOwnerPersonId.getId,
            name = Just name,
            mobileNo = mobileNo',
            roleName = Just (show fleetOwner.role),
            isEligibleForSubscription = Just isEligibleForSubscription,
            ..
          }

---------------------------------------------------------------------
data FleetOwnerInfo = FleetOwnerInfo
  { fleetOwner :: DP.Person,
    mbOperator :: Maybe DP.Person
  }

checkRequestorAccessToFleet ::
  Maybe Text ->
  Text ->
  Flow FleetOwnerInfo
checkRequestorAccessToFleet mbRequestorId fleetOwnerId = do
  fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")
  case mbRequestorId of
    Nothing -> pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing} -- old flow
    Just requestorId -> do
      -- new flow
      requestor <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
      case requestor.role of
        DP.FLEET_OWNER -> do
          unless (fleetOwner.id == requestor.id) $
            throwError (InvalidRequest "Invalid fleet owner")
          pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing}
        DP.OPERATOR -> do
          association <-
            QFleetOperatorAssociation.findByFleetIdAndOperatorId fleetOwner.id.getId requestor.id.getId True
              >>= fromMaybeM (InvalidRequest "FleetOperatorAssociation does not exist") -- TODO add error codes
          whenJust association.associatedTill $ \associatedTill -> do
            now <- getCurrentTime
            when (now > associatedTill) $
              throwError (InvalidRequest "FleetOperatorAssociation expired")
          pure $ FleetOwnerInfo {fleetOwner, mbOperator = Just requestor}
        DP.ADMIN -> pure $ FleetOwnerInfo {fleetOwner, mbOperator = Nothing}
        _ -> throwError AccessDenied

postDriverFleetSendJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Common.AuthReq ->
  Flow Common.AuthRes
postDriverFleetSendJoiningOtp merchantShortId opCity fleetOwnerName mbFleetOwnerId mbRequestorId req = do
  let phoneNumber = req.mobileCountryCode <> req.mobileNumber
  sendOtpRateLimitOptions <- asks (.sendOtpRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeFleetDriverHitsCountKey phoneNumber) sendOtpRateLimitOptions

  merchant <- findMerchantByShortId merchantShortId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> do
    void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId
    DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  smsCfg <- asks (.smsCfg)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  MobileValidation.validateMobileNumber transporterConfig req.mobileNumber req.mobileCountryCode merchantOpCity.country
  mobileNumberHash <- getDbHash req.mobileNumber
  mbPerson <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER
  case mbPerson of
    Nothing -> DRBReg.auth merchantShortId opCity req -------------- to onboard a driver that is not the part of the fleet
    Just person -> do
      withLogTag ("personId_" <> getId person.id) $ do
        SA.checkForDriverAssociationOverwrite merchant person.id
        let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp

        otpCode <- maybe generateOTPCode return useFakeOtpM
        whenNothing_ useFakeOtpM $ do
          (mbSender, message, templateId) <-
            MessageBuilder.buildFleetJoiningMessage merchantOpCityId $
              MessageBuilder.BuildFleetJoiningMessageReq
                { otp = otpCode,
                  fleetOwnerName = fleetOwnerName
                }
          let sender = fromMaybe smsCfg.sender mbSender
          Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult
        let key = makeFleetDriverOtpKey phoneNumber
        Redis.setExp key otpCode 3600
      pure $ Common.AuthRes {authId = "ALREADY_USING_APPLICATION", attempts = 0}

---------------------------------------------------------------------
postDriverFleetVerifyJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Common.VerifyFleetJoiningOtpReq ->
  Flow APISuccess
postDriverFleetVerifyJoiningOtp merchantShortId opCity fleetOwnerId mbAuthId mbRequestorId req = do
  FleetOwnerInfo {fleetOwner, mbOperator} <- checkRequestorAccessToFleet mbRequestorId fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound req.mobileNumber)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  MobileValidation.validateMobileNumber transporterConfig req.mobileNumber req.mobileCountryCode merchantOpCity.country
  case mbAuthId of
    Just authId -> do
      smsCfg <- asks (.smsCfg)
      deviceToken <- fromMaybeM (DeviceTokenNotFound) $ req.deviceToken

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig person

      void $ DRBReg.verify authId True fleetOwnerId (mbOperator <&> (.id)) transporterConfig Common.AuthVerifyReq {otp = req.otp, deviceToken = deviceToken}

      whenJust mbOperator $ \referredOperator -> do
        DOR.makeDriverReferredByOperator merchantOpCityId person.id referredOperator.id

      let phoneNumber = req.mobileCountryCode <> req.mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
        whenNothing_ useFakeOtpM $
          do
            (mbSender, message, templateId) <-
              MessageBuilder.buildFleetJoinAndDownloadAppMessage merchantOpCityId $
                MessageBuilder.BuildDownloadAppMessageReq
                  { fleetOwnerName = fleetOwner.firstName
                  }
            let sender = fromMaybe smsCfg.sender mbSender
            Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId)
            >>= Sms.checkSmsResult
    Nothing -> do
      let key = makeFleetDriverOtpKey (req.mobileCountryCode <> req.mobileNumber)
      otp <- Redis.get key >>= fromMaybeM OtpNotFound
      when (otp /= req.otp) $ throwError InvalidOtp
      checkAssoc <- B.runInReplica $ QFDV.findByDriverIdAndFleetOwnerId person.id fleetOwnerId True
      when (isJust checkAssoc) $ throwError (InvalidRequest "Driver already associated with fleet")

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId transporterConfig person

      -- Check if driver has any active rides (not completed or cancelled)
      mbActiveRide <- B.runInReplica $ QRideExtra.getUpcomingOrActiveByDriverId person.id
      when (isJust mbActiveRide) $ throwError (InvalidRequest "Driver has active rides. Please complete or cancel all rides before adding to fleet")

      -- onboarded operator required only for new drivers
      assoc <- FDA.makeFleetDriverAssociation person.id fleetOwnerId Nothing (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QFDV.create assoc
      when (transporterConfig.deleteDriverBankAccountWhenLinkToFleet == Just True) $ QDBA.deleteById person.id
      Analytics.handleDriverAnalyticsAndFlowStatus
        transporterConfig
        person.id
        Nothing
        ( \driverInfo -> do
            Analytics.incrementFleetOwnerAnalyticsActiveDriverCount (Just fleetOwnerId) person.id
            mOperator <- FOV.findByFleetOwnerId fleetOwnerId True
            when (isNothing mOperator) $ logTagError "AnalyticsAddDriver" "Operator not found for fleet owner"
            whenJust mOperator $ \operator -> do
              when driverInfo.enabled $ Analytics.incrementOperatorAnalyticsDriverEnabled transporterConfig operator.operatorId
              Analytics.incrementOperatorAnalyticsActiveDriver transporterConfig operator.operatorId
        )
        ( \driverInfo -> do
            DDriverMode.incrementFleetOperatorStatusKeyForDriver DP.FLEET_OWNER fleetOwnerId driverInfo.driverFlowStatus
        )

  pure Success

makeFleetDriverOtpKey :: Text -> Text
makeFleetDriverOtpKey phoneNo = "Fleet:Driver:PhoneNo" <> phoneNo

makeFleetDriverHitsCountKey :: Text -> Text
makeFleetDriverHitsCountKey phoneNo = "Fleet:Driver:PhoneNoHits" <> phoneNo <> ":hitsCount"

---------------------------------------------------------------------
postDriverFleetLinkRCWithDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Common.LinkRCWithDriverForFleetReq ->
  Flow APISuccess
postDriverFleetLinkRCWithDriver merchantShortId opCity fleetOwnerId mbRequestorId req = do
  void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  phoneNumberHash <- getDbHash req.driverMobileNumber
  let mobileCountryCode = fromMaybe (P.getCountryMobileCode merchantOpCity.country) req.driverMobileCountryCode
  driver <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneNumberHash merchant.id DP.DRIVER >>= fromMaybeM (DriverNotFound req.driverMobileNumber)
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist driver.id.getId)
  rc <- RCQuery.findLastVehicleRCWrapper req.vehicleRegistrationNumber >>= fromMaybeM (RCNotFound req.vehicleRegistrationNumber)
  when (isNothing rc.fleetOwnerId || (isJust rc.fleetOwnerId && rc.fleetOwnerId /= Just fleetOwnerId)) $ throwError VehicleNotPartOfFleet
  unless (rc.verificationStatus == Documents.VALID) $ throwError (RcNotValid)
  validateFleetDriverAssociation fleetOwnerId driver.id
  isValidAssociation <- checkRCAssociationForDriver driver.id (Just rc) False
  when (not isValidAssociation) $ do
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    allLinkedRCs <- DAQuery.findAllLinkedByDriverId driver.id
    unless (length allLinkedRCs < transporterConfig.rcLimit) $ throwError (RCLimitReached transporterConfig.rcLimit)
    createDriverRCAssociationIfPossible transporterConfig driver.id rc
  return Success

---------------------------------------------------------------------
postDriverDashboardFleetWmbTripEnd ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.TripTransaction ->
  Text ->
  Maybe Common.ActionSource ->
  Flow APISuccess
postDriverDashboardFleetWmbTripEnd _ _ tripTransactionId fleetOwnerId mbTerminationSource = do
  fleetConfig <- QFC.findByPrimaryKey (Id fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetOwnerId)
  tripTransaction <- QTT.findByTransactionId (cast tripTransactionId) >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
  mbCurrentDriverLocation <-
    withTryCatch "driversLocation:postDriverDashboardFleetWmbTripEnd" (LF.driversLocation [tripTransaction.driverId])
      >>= \case
        Left _ -> do
          logError "Driver is not active since 24 hours, please ask driver to go online and then end the trip."
          return Nothing
        Right locations -> do
          let location = listToMaybe locations
          when (isNothing location) $ logError "Driver is not active since 24 hours, please ask driver to go online and then end the trip."
          return location
  void $ WMB.cancelTripTransaction fleetConfig tripTransaction (maybe (LatLong 0.0 0.0) (\currentDriverLocation -> LatLong currentDriverLocation.lat currentDriverLocation.lon) mbCurrentDriverLocation) (castActionSource mbTerminationSource)
  pure Success

castActionSource :: Maybe Common.ActionSource -> DTT.ActionSource
castActionSource actionSource = case actionSource of
  Just Common.DriverDirect -> DTT.DriverDirect
  Just Common.DriverOnApproval -> DTT.DriverOnApproval
  Just Common.AutoDetect -> DTT.AutoDetect
  Just Common.Dashboard -> DTT.Dashboard
  Just Common.ForceDashboard -> DTT.ForceDashboard
  Just Common.CronJob -> DTT.CronJob
  Nothing -> DTT.Dashboard

---------------------------------------------------------------------

getDriverFleetGetAllBadge :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe DFBT.FleetBadgeType -> Maybe Bool -> Maybe Text -> Flow Common.FleetBadgeResT
getDriverFleetGetAllBadge merchantShortId opCity limit offset mbSearchString mbFleetOwnerId mbBadgeType mbIsActive mbMemberPersonId = do
  memberPersonId <- mbMemberPersonId & fromMaybeM (InvalidRequest "Member Person ID is required")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId mbFleetOwnerId
  let fleetOwnerIds = map fst fleetOwnerInfo
      fleetNameMap = Map.fromList fleetOwnerInfo
  case mbIsActive of
    Just True -> do
      activeBadges <- QFBA.findAllActiveFleetBadgeAssociation limit offset mbSearchString mbBadgeType
      fleetBadgeInfos <-
        mapM
          ( \(_, badge) -> do
              pure $
                Common.FleetBadgesAPIEntityT
                  { badgeName = badge.badgeName,
                    isActive = True,
                    badgeType = badge.badgeType,
                    fleetOwnerId = badge.fleetOwnerId.getId,
                    fleetOwnerName = fromMaybe "" (Map.lookup badge.fleetOwnerId.getId fleetNameMap)
                  }
          )
          activeBadges
      return $ Common.FleetBadgeResT {..}
    Just False -> do
      inactiveBadges <- QFBA.findAllInactiveFleetBadgeAssociation limit offset mbSearchString mbBadgeType
      fleetBadgeInfos <-
        mapM
          ( \(_, badge) -> do
              pure $
                Common.FleetBadgesAPIEntityT
                  { badgeName = badge.badgeName,
                    isActive = False,
                    badgeType = badge.badgeType,
                    fleetOwnerId = badge.fleetOwnerId.getId,
                    fleetOwnerName = fromMaybe "" (Map.lookup badge.fleetOwnerId.getId fleetNameMap)
                  }
          )
          inactiveBadges
      return $ Common.FleetBadgeResT {..}
    Nothing -> do
      fleetBadgesByOwner <- QFB.findAllMatchingBadgesMultiFleetOwner mbSearchString (toInteger <$> limit) (toInteger <$> offset) merchantOpCity.id fleetOwnerIds mbBadgeType
      fleetBadgeInfos <-
        mapM
          ( \badge -> do
              driverBadgeAssociation <- QFBA.findActiveFleetBadgeAssociationById badge.id badge.badgeType
              pure $
                Common.FleetBadgesAPIEntityT
                  { badgeName = badge.badgeName,
                    isActive = isJust driverBadgeAssociation,
                    badgeType = badge.badgeType,
                    fleetOwnerId = badge.fleetOwnerId.getId,
                    fleetOwnerName = fromMaybe "" (Map.lookup badge.fleetOwnerId.getId fleetNameMap)
                  }
          )
          fleetBadgesByOwner
      return $ Common.FleetBadgeResT {..}

---------------------------------------------------------------------
getDriverFleetRoutes ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe LatLong ->
  Maybe Text ->
  Int ->
  Int ->
  Flow Common.RouteAPIResp
getDriverFleetRoutes merchantShortId opCity fleetOwnerId mbCurrentLocation mbSearchString limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let vehicleType = DVC.BUS
  fleetRoutesByOwner <- QFRA.findAllByFleetOwnerIdAndCityId (Id fleetOwnerId) merchantOpCity.id
  let existedRouteCodes = map (.routeCode) fleetRoutesByOwner
  allRoutes <- case mbSearchString of
    Just searchString -> QRoute.findAllMatchingRoutes (Just searchString) (Just $ toInteger limit) (Just $ toInteger offset) merchantOpCity.id vehicleType existedRouteCodes
    Nothing -> QRoute.findAllByMerchantOperatingCityAndVehicleType (Just limit) (Just offset) merchantOpCity.id vehicleType existedRouteCodes
  listItem <- case mbCurrentLocation of
    Nothing -> return $ map (\route -> createRouteRespItem route Nothing) allRoutes
    Just startLocation -> do
      sortedRoutesWithDistance <- getRoutesByLocation merchant.id merchantOpCity.id startLocation allRoutes
      return $ map (\rData -> createRouteRespItem rData.route rData.distance) sortedRoutesWithDistance
  let summary = Common.Summary {totalCount = 10000, count = length listItem}
  pure $ Common.RouteAPIResp {listItem = listItem, summary = summary}

data RouteData = RouteData
  { route :: DRoute.Route,
    distance :: Maybe Meters
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data RouteDetails = RouteDetails
  { lat :: Double,
    lon :: Double,
    route :: DRoute.Route
  }
  deriving (Generic, Show, HasCoordinates, ToJSON, FromJSON)

createRouteRespItem :: DRoute.Route -> Maybe Meters -> Common.RouteRespItem
createRouteRespItem route distance = do
  Common.RouteRespItem
    { code = route.code,
      shortName = route.shortName,
      longName = route.longName,
      startPoint = route.startPoint,
      endPoint = route.endPoint,
      distance = distance,
      totalStops = Nothing,
      waypoints = Nothing,
      timeBounds = Just route.timeBounds
    }

getRoutesByLocation :: () => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> [DRoute.Route] -> Flow [RouteData]
getRoutesByLocation merchantId merchantOpCityId origin routes_ = do
  if null routes_
    then return []
    else do
      let routes = map mkRouteWithLatLon routes_
      let maxBatchSize = 100
          routeBatches = chunksOf maxBatchSize routes
      batchedResults <- fmap concat $
        forM routeBatches $ \batch -> do
          res <-
            withTryCatch "getDistances:getRoutesByLocation" $
              Maps.getDistances merchantId merchantOpCityId Nothing $
                Maps.GetDistancesReq
                  { origins = fromList batch,
                    destinations = fromList [origin],
                    distanceUnit = Meter,
                    sourceDestinationMapping = Nothing,
                    travelMode = Just Maps.CAR
                  }
          case res of
            Left _ -> return $ map (\routeDetails -> RouteData {route = routeDetails.route, distance = Nothing}) batch
            Right routeDistanceResp -> return $ map (\routeWithDistance -> RouteData {route = routeWithDistance.origin.route, distance = Just routeWithDistance.distance}) (toList routeDistanceResp)
      let sortedRoutes = sortOn (.distance) batchedResults
      pure sortedRoutes
  where
    mkRouteWithLatLon route =
      RouteDetails
        { lat = route.startPoint.lat,
          lon = route.startPoint.lon,
          route = route
        }

---------------------------------------------------------------------
getDriverFleetPossibleRoutes ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow Common.RouteAPIResp
getDriverFleetPossibleRoutes merchantShortId opCity fleetOwnerId startStopCode = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  now <- getCurrentTime
  let day = dayOfWeek (utctDay now)
  fleetRoutesByOwner <- QFRA.findAllByFleetOwnerIdAndCityId (Id fleetOwnerId) merchanOperatingCity.id
  let existedRouteCodes = map (.routeCode) fleetRoutesByOwner
  possibleRoutes <- QRTSM.findAllByStopCodeAndStopSequenceAndRoutes startStopCode 1 1 day existedRouteCodes
  listItem <-
    mapM
      ( \rtsmData -> do
          route <- QRoute.findByRouteCode rtsmData.routeCode >>= fromMaybeM (RouteNotFound rtsmData.routeCode)
          pure $ createRouteRespItem route Nothing
      )
      possibleRoutes
  let summary = Common.Summary {totalCount = 10000, count = length listItem}
  pure $ Common.RouteAPIResp {listItem = listItem, summary = summary}

---------------------------------------------------------------------
postDriverFleetTripPlanner ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.TripPlannerReq ->
  Flow APISuccess
postDriverFleetTripPlanner merchantShortId opCity fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  fleetConfig <- QFC.findByPrimaryKey (Id fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetOwnerId)
  mbDriverBadge <-
    case req.driverBadgeName <|> req.badgeName of
      Just driverBadgeName -> do
        driverBadge <- WMB.validateBadgeAssignment (cast req.driverId) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId driverBadgeName DFBT.DRIVER
        WMB.linkFleetBadge (cast req.driverId) merchant.id merchantOpCity.id fleetOwnerId driverBadge DFBT.DRIVER
        return $ Just driverBadge
      Nothing -> pure Nothing
  mbConductorBadge <-
    case req.conductorBadgeName of
      Just conductorBadgeName -> do
        conductorBadge <- WMB.validateBadgeAssignment (cast req.driverId) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId conductorBadgeName DFBT.CONDUCTOR
        WMB.linkFleetBadge (cast req.driverId) merchant.id merchantOpCity.id fleetOwnerId conductorBadge DFBT.CONDUCTOR
        return $ Just conductorBadge
      Nothing -> pure Nothing
  mbPilotBadge <-
    case req.pilotBadgeName of
      Just pilotBadgeName -> do
        pilotBadge <- WMB.validateBadgeAssignment (cast req.driverId) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId pilotBadgeName DFBT.OFFICER
        WMB.linkFleetBadge (cast req.driverId) merchant.id merchantOpCity.id fleetOwnerId pilotBadge DFBT.OFFICER
        return $ Just pilotBadge
      Nothing -> pure Nothing
  withTryCatch
    "validateVehicleAssignment:postDriverFleetTripPlanner"
    ( do
        vehicleRC <- WMB.validateVehicleAssignment (cast req.driverId) req.vehicleNumber merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId
        WMB.linkVehicleToDriver (cast req.driverId) merchant.id merchantOpCity.id fleetConfig fleetOwnerId req.vehicleNumber vehicleRC
        return vehicleRC
    )
    >>= \case
      Left err -> do
        _ <- WMB.unlinkFleetBadgeFromDriver (cast req.driverId)
        throwError (InternalError $ "Something went wrong while linking vehicle to driver. Error: " <> (T.pack $ displayException err))
      Right vehicleRC -> do
        createTripTransactions merchant.id merchantOpCity.id fleetOwnerId req.driverId vehicleRC (mbDriverBadge <|> mbPilotBadge) mbConductorBadge req.trips
        pure Success

createTripTransactions :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Id Common.Driver -> DVRC.VehicleRegistrationCertificate -> Maybe DFB.FleetBadge -> Maybe DFB.FleetBadge -> [Common.TripDetails] -> Flow ()
createTripTransactions merchantId merchantOpCityId fleetOwnerId driverId vehicleRC mbDriverBadge mbConductorBadge trips = do
  initialActiveTrip <- WMB.findNextActiveTripTransaction fleetOwnerId (cast driverId)
  let (isActive, mbScheduledTripTime) = case initialActiveTrip of
        Just tripTransaction -> (True, if tripTransaction.status == DTT.TRIP_ASSIGNED then tripTransaction.scheduledTripTime else Nothing)
        Nothing -> (False, Nothing)
  (allTransactions, _, mbLeastScheduledTripTime) <-
    foldM
      ( \(accTransactions, updatedIsActive, mbScheduledTripTime') trip -> do
          ( if updatedIsActive
              then do
                case trip.tripType of
                  Just Common.PILOT -> do
                    if fromMaybe False ((>) <$> mbScheduledTripTime' <*> trip.scheduledTripTime)
                      then do
                        tripTransactions <- makeTripTransactions trip DTT.TRIP_ASSIGNED
                        return (accTransactions <> tripTransactions, updatedIsActive, trip.scheduledTripTime)
                      else do
                        tripTransactions <- makeTripTransactions trip DTT.UPCOMING
                        return (accTransactions <> tripTransactions, updatedIsActive, mbScheduledTripTime')
                  _ -> do
                    tripTransactions <- makeTripTransactions trip DTT.UPCOMING
                    return (accTransactions <> tripTransactions, updatedIsActive, mbScheduledTripTime')
              else do
                tripTransactions <- makeTripTransactions trip DTT.TRIP_ASSIGNED
                return (accTransactions <> tripTransactions, True, mbScheduledTripTime')
            )
      )
      ([], isActive, mbScheduledTripTime)
      (sortOn (.scheduledTripTime) trips)
  WMB.findNextActiveTripTransaction fleetOwnerId (cast driverId)
    >>= \case
      Just tTran -> do
        when (fromMaybe False ((<) <$> mbLeastScheduledTripTime <*> tTran.scheduledTripTime)) $ QTT.updateStatus DTT.UPCOMING tTran.id
        QTT.createMany allTransactions
      Nothing -> do
        QTT.createMany allTransactions
        whenJust (listToMaybe allTransactions) $ \tripTransaction -> do
          case tripTransaction.tripType of
            Just DTT.PILOT -> do
              psource <- maybe (throwError (InternalError "Pilot source not found")) pure tripTransaction.pilotSource
              pdestination <- maybe (throwError (InternalError "Pilot destination not found")) pure tripTransaction.pilotDestination
              WMB.postAssignTripTransaction tripTransaction Nothing True psource psource pdestination True
            _ -> do
              route <- QRoute.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
              (routeSourceStopInfo, routeDestinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route route.code
              WMB.postAssignTripTransaction tripTransaction (Just route) True routeSourceStopInfo.point routeSourceStopInfo.point routeDestinationStopInfo.point True
  where
    pilotCode :: Text
    pilotCode = "PILOT"

    makeTripTransactions :: Common.TripDetails -> DTT.TripStatus -> Flow [DTT.TripTransaction]
    makeTripTransactions trip tripStatus = do
      let tripType' = fromMaybe Common.WIMB trip.tripType
      case tripType' of
        Common.WIMB -> do
          route <- QRoute.findByRouteCode trip.routeCode >>= fromMaybeM (RouteNotFound trip.routeCode)
          (_, routeDestinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route route.code
          case (trip.roundTrip, route.roundRouteCode) of
            (Just _, Nothing) -> throwError (RoundTripNotAllowedForRoute route.code)
            (Just roundTrip, Just roundRouteCode) -> do
              when (roundTrip.frequency == 0) $ throwError RoundTripFrequencyShouldBeGreaterThanZero
              roundRoute <- QRoute.findByRouteCode roundRouteCode >>= fromMaybeM (RouteNotFound trip.routeCode)
              (_, roundRouteDestinationStopInfo) <- WMB.getSourceAndDestinationStopInfo roundRoute roundRouteCode
              foldM
                ( \accTransactions freqIdx -> do
                    let (routeCode, roundRouteCode_, endStopCode) = bool (roundRouteCode, route.code, roundRouteDestinationStopInfo.code) (route.code, roundRouteCode, routeDestinationStopInfo.code) (freqIdx `mod` 2 == 0)
                    tripTransaction <- mkTransaction routeCode (Just roundRouteCode_) endStopCode tripStatus trip
                    pure $ accTransactions <> [tripTransaction]
                )
                []
                [0 .. (2 * roundTrip.frequency) -1]
            (_, _) -> do
              tripTransaction <- mkTransaction route.code route.roundRouteCode routeDestinationStopInfo.code tripStatus trip
              pure [tripTransaction]
        Common.PILOT -> do
          tripTransaction <- mkTransaction pilotCode Nothing pilotCode tripStatus trip
          pure [tripTransaction]

    mkTransaction :: Text -> Maybe Text -> Text -> DTT.TripStatus -> Common.TripDetails -> Flow DTT.TripTransaction
    mkTransaction routeCode roundRouteCode endStopCode tripStatus trip = do
      transactionId <- generateGUID
      now <- getCurrentTime
      vehicleNumber <- decrypt vehicleRC.certificateNumber
      return $
        DTT.TripTransaction
          { allowEndingMidRoute = False,
            deviationCount = 0,
            driverId = cast driverId,
            endLocation = Nothing,
            endStopCode = endStopCode,
            fleetOwnerId = Id fleetOwnerId,
            id = transactionId,
            isCurrentlyDeviated = False,
            startLocation = Nothing,
            startedNearStopCode = Nothing,
            status = tripStatus,
            routeCode = routeCode,
            roundRouteCode = roundRouteCode,
            tripCode = Nothing,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = maybe DrInfo.BUS_NON_AC DV.castVariantToServiceTier vehicleRC.vehicleVariant,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            tripStartSource = Nothing,
            tripTerminationSource = Nothing,
            endRideApprovalRequestId = Nothing,
            createdAt = now,
            updatedAt = now,
            driverName = mbDriverBadge <&> (.badgeName),
            driverFleetBadgeId = mbDriverBadge <&> (.id),
            conductorName = mbConductorBadge <&> (.badgeName),
            conductorFleetBadgeId = mbConductorBadge <&> (.id),
            dutyType = trip.dutyType,
            vipName = trip.vipName,
            startAddress = trip.startAddress >>= (.address),
            endAddress = trip.endAddress >>= (.address),
            scheduledTripTime = trip.scheduledTripTime,
            pilotSource = trip.startAddress <&> (.point),
            pilotDestination = trip.endAddress <&> (.point),
            tripType = castTripType <$> trip.tripType,
            tripEstimatedRouteDetails = trip.estimatedRouteDetails >>= (\estimatedRoute -> Just (DTT.EstimatedRouteDetails {distance = estimatedRoute.distance, duration = estimatedRoute.duration, polyline = estimatedRoute.polyline})),
            ..
          }

    castTripType = \case
      Common.WIMB -> DTT.WIMB
      Common.PILOT -> DTT.PILOT

---------------------------------------------------------------------
getDriverFleetTripTransactions :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Int -> Flow Common.TripTransactionRespT
getDriverFleetTripTransactions merchantShortId opCity driverId mbFrom mbTo mbVehicleNumber mbFleetOwnerId mbMemberPersonId limit offset = do
  memberPersonId <- mbMemberPersonId & fromMaybeM (InvalidRequest "Member Person ID is required")
  merchant <- findMerchantByShortId merchantShortId
  _ <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId mbFleetOwnerId
  let fleetNameMap = Map.fromList fleetOwnerInfo
      fleetOwnerIds = map fst fleetOwnerInfo
  tripTransactions <- QTT.findAllTripTransactionByDriverIdWithinCreationRangeMultiFleetOwner fleetOwnerIds (Just limit) (Just offset) (cast driverId) mbFrom mbTo mbVehicleNumber
  let trips = map (buildTripTransactionDetailsT fleetNameMap) tripTransactions
      summary = Common.Summary {totalCount = 10000, count = length trips}
  pure $ Common.TripTransactionRespT {trips = trips, totalTrips = length tripTransactions, summary = summary}
  where
    buildTripTransactionDetailsT fleetNameMap tripTransaction =
      Common.TripTransactionDetailT
        { tripTransactionId = cast tripTransaction.id,
          routeCode = tripTransaction.routeCode,
          tripStartTime = tripTransaction.tripStartTime,
          tripEndTime = tripTransaction.tripEndTime,
          tripStatus = castTripStatus tripTransaction.status,
          fleetOwnerId = tripTransaction.fleetOwnerId.getId,
          fleetOwnerName = fromMaybe "" (Map.lookup (tripTransaction.fleetOwnerId.getId) fleetNameMap),
          tripType = castTripType <$> tripTransaction.tripType,
          scheduledTripTime = tripTransaction.scheduledTripTime,
          dutyType = tripTransaction.dutyType,
          vipName = tripTransaction.vipName,
          startAddress = tripTransaction.pilotSource >>= (\point -> Just (Common.Address {point = point, address = tripTransaction.startAddress})),
          endAddress = tripTransaction.pilotDestination >>= (\point -> Just (Common.Address {point = point, address = tripTransaction.endAddress})),
          estimatedRouteDetails = tripTransaction.tripEstimatedRouteDetails >>= (\estimatedRoute -> Just (Common.EstimatedRouteDetails {distance = estimatedRoute.distance, duration = estimatedRoute.duration, polyline = estimatedRoute.polyline}))
        }
    castTripType = \case
      DTT.PILOT -> Common.PILOT
      DTT.WIMB -> Common.WIMB
    castTripStatus = \case
      TRIP_ASSIGNED -> Common.TRIP_ASSIGNED
      IN_PROGRESS -> Common.IN_PROGRESS
      PAUSED -> Common.PAUSED
      COMPLETED -> Common.COMPLETED
      CANCELLED -> Common.CANCELLED
      UPCOMING -> Common.UPCOMING

---------------------------------------------------------------------
data CreateDriverBusRouteMappingCSVRow = CreateDriverBusRouteMappingCSVRow
  { driverId :: Text,
    vehicleNumber :: Text,
    routeCode :: Text,
    roundTripFreq :: Text,
    driverBadgeName :: Maybe Text,
    conductorBadgeName :: Maybe Text
  }

data DriverBusRouteDetails = DriverBusRouteDetails
  { driverPhoneNo :: Text,
    vehicleNumber :: Text,
    routeCode :: Text,
    roundTripFreq :: Maybe Int,
    driverBadgeName :: Maybe Text,
    conductorBadgeName :: Maybe Text
  }

instance FromNamedRecord CreateDriverBusRouteMappingCSVRow where
  parseNamedRecord r =
    CreateDriverBusRouteMappingCSVRow
      <$> r .: "driver_phone_number"
      <*> r .: "vehicle_number"
      <*> r .: "route_code"
      <*> r .: "round_trip_freq"
      <*> (Just <$> r .: "driver_badge_name" <|> pure Nothing)
      <*> (Just <$> r .: "conductor_badge_name" <|> pure Nothing)

-- <*> r .: "force_assign"

postDriverFleetAddDriverBusRouteMapping ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreateDriverBusRouteMappingReq ->
  Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDriverBusRouteMapping merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  fleetOwnerId <- case req.fleetOwnerId of
    Nothing -> throwError FleetOwnerIdRequired
    Just id -> pure id
  fleetConfig <- QFC.findByPrimaryKey (Id fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetOwnerId)
  driverBusRouteDetails <- Csv.readCsv @CreateDriverBusRouteMappingCSVRow @DriverBusRouteDetails req.file parseMappingInfo

  when (length driverBusRouteDetails > 100) $ throwError $ MaxDriversLimitExceeded 100
  let groupedDetails = groupBy (\a b -> a.driverPhoneNo == b.driverPhoneNo) $ sortOn (.driverPhoneNo) driverBusRouteDetails
      mobileCountryCode = P.getCountryMobileCode merchantOpCity.country

  driverTripPlanner <-
    mapM
      ( \case
          [] -> throwError (InternalError "Something Went Wrong while grouping Drivers")
          (driverGroup : driverGroups) -> do
            mobileNumberHash <- getDbHash driverGroup.driverPhoneNo
            person <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumberHash merchant.id DP.DRIVER
            case person of
              Nothing -> throwError (InvalidRequest $ "Driver with Mobile Number (" <> driverGroup.driverPhoneNo <> ") Not Found, Please Add the Driver to the fleet and Try Again!")
              Just driver -> do
                let vehicleNumber = driverGroup.vehicleNumber
                    tripPlannerRequests = map makeTripPlannerReq ([driverGroup] <> driverGroups)
                return (driver, driverGroup.driverPhoneNo, driverGroup.driverBadgeName, driverGroup.conductorBadgeName, vehicleNumber, tripPlannerRequests)
      )
      groupedDetails

  unprocessedEntities <-
    foldlM
      ( \unprocessedEntities (driver, driverMobileNumber, mbDriverBadgeName, mbConductorBadgeName, vehicleNumber, tripPlannerRequests) -> do
          withTryCatch
            "validateVehicleAssignment:postDriverFleetAddDriverBusRouteMapping"
            ( do
                vehicleRC <- WMB.validateVehicleAssignment (cast driver.id) vehicleNumber merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId
                driverBadge <-
                  case mbDriverBadgeName of
                    Just driverBadgeName -> do
                      badge <- WMB.validateBadgeAssignment (cast driver.id) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId driverBadgeName DFBT.DRIVER
                      return $ Just badge
                    Nothing -> return Nothing
                conductorBadge <-
                  case mbConductorBadgeName of
                    Just conductorBadgeName -> do
                      badge <- WMB.validateBadgeAssignment (cast driver.id) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId conductorBadgeName DFBT.CONDUCTOR
                      return $ Just badge
                    Nothing -> return Nothing
                return (vehicleRC, driverBadge, conductorBadge)
            )
            >>= \case
              Left err -> return $ unprocessedEntities <> ["Unable to link vehicle to the Driver (" <> driverMobileNumber <> "): " <> (T.pack $ displayException err)]
              Right (vehicleRC, mbDriverBadge, mbConductorBadge) -> do
                withTryCatch
                  "linkVehicleToDriver:postDriverFleetAddDriverBusRouteMapping"
                  ( do
                      WMB.linkVehicleToDriver (cast driver.id) merchant.id merchantOpCity.id fleetConfig fleetConfig.fleetOwnerId.getId vehicleNumber vehicleRC
                      whenJust mbDriverBadge $ \driverBadge -> WMB.linkFleetBadge (cast driver.id) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId driverBadge DFBT.DRIVER
                      whenJust mbConductorBadge $ \conductorBadge -> WMB.linkFleetBadge (cast driver.id) merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId conductorBadge DFBT.CONDUCTOR
                      createTripTransactions merchant.id merchantOpCity.id fleetConfig.fleetOwnerId.getId (cast driver.id) vehicleRC mbDriverBadge mbConductorBadge tripPlannerRequests
                  )
                  >>= \case
                    Left err -> do
                      void $ WMB.unlinkVehicleToDriver (cast driver.id) merchant.id merchantOpCity.id vehicleNumber
                      void $ WMB.unlinkFleetBadgeFromDriver (cast driver.id)
                      return $ unprocessedEntities <> ["Unable to create Trip Transactions for Driver (" <> driverMobileNumber <> "): " <> (T.pack $ displayException err)]
                    Right _ -> return unprocessedEntities
      )
      []
      driverTripPlanner
  pure $ Common.APISuccessWithUnprocessedEntities unprocessedEntities
  where
    parseMappingInfo :: Int -> CreateDriverBusRouteMappingCSVRow -> Flow DriverBusRouteDetails
    parseMappingInfo idx row = do
      driverPhoneNo <- Csv.cleanCSVField idx row.driverId "Driver Phone number"
      vehicleNumber <- Csv.cleanCSVField idx row.vehicleNumber "Vehicle number"
      routeCode <- Csv.cleanCSVField idx row.routeCode "Route code"
      let roundTripFreq = Csv.readMaybeCSVField idx row.roundTripFreq "Round trip freq"
      driverBadgeName <-
        case row.driverBadgeName of
          Just driverBadgeName -> Just <$> Csv.cleanCSVField idx driverBadgeName "Driver Badge name"
          Nothing -> pure Nothing
      conductorBadgeName <-
        case row.conductorBadgeName of
          Just conductorBadgeName -> Just <$> Csv.cleanCSVField idx conductorBadgeName "Conductor Badge name"
          Nothing -> pure Nothing
      pure $ DriverBusRouteDetails driverPhoneNo vehicleNumber routeCode roundTripFreq driverBadgeName conductorBadgeName

    makeTripPlannerReq driverGroup =
      Common.TripDetails
        { routeCode = driverGroup.routeCode,
          roundTrip = fmap (\freq -> Common.RoundTripDetail {frequency = freq}) driverGroup.roundTripFreq,
          scheduledTripTime = Nothing,
          vipName = Nothing,
          dutyType = Nothing,
          startAddress = Nothing,
          endAddress = Nothing,
          tripType = Nothing,
          estimatedRouteDetails = Nothing
        }

---------------------------------------------------------------------
data CreateDriversCSVRow = CreateDriversCSVRow
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe Text,
    fleetPhoneNo :: Maybe Text,
    badgeName :: Maybe Text,
    badgeRank :: Maybe Text,
    badgeType :: Maybe Text
  }

data DriverDetails = DriverDetails
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory,
    fleetPhoneNo :: Maybe Text,
    badgeName :: Maybe Text,
    badgeRank :: Maybe Text,
    badgeType :: Maybe DFBT.FleetBadgeType
  }

instance FromNamedRecord CreateDriversCSVRow where
  parseNamedRecord r =
    CreateDriversCSVRow
      <$> r .: "driver_name"
      <*> r .: "driver_phone_number"
      <*> optional (r .: "driver_onboarding_vehicle_category")
      <*> optional (r .: "fleet_phone_no")
      <*> optional (r .: "badge_name")
      <*> optional (r .: "badge_rank")
      <*> optional (r .: "badge_type")

postDriverFleetAddDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Common.CreateDriversReq ->
  Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDrivers merchantShortId opCity mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  driverDetails <- Csv.readCsv @CreateDriversCSVRow @DriverDetails req.file parseDriverInfo
  when (length driverDetails > 100) $ throwError $ MaxDriversLimitExceeded 100 -- TODO: Configure the limit
  let process func =
        foldlM
          ( \unprocessedEntities driverDetail -> do
              withTryCatch
                "process:postDriverFleetAddDrivers"
                (func driverDetail)
                >>= \case
                  Left err -> return $ unprocessedEntities <> ["Unable to add Driver (" <> driverDetail.driverPhoneNumber <> ") to the Fleet: " <> T.pack (displayException err)]
                  Right _ -> return unprocessedEntities
          )
          []
          driverDetails

  unprocessedEntities <- case mbRequestorId of
    Nothing -> do
      -- old flow
      fleetOwnerId <- case req.fleetOwnerId of
        Nothing -> throwError FleetOwnerIdRequired
        Just id -> pure id
      DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
      fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
      unless (fleetOwner.role == DP.FLEET_OWNER) $
        throwError (InvalidRequest "Invalid fleet owner")
      process (processDriverByFleetOwner merchant merchantOpCity transporterConfig fleetOwner)
    Just requestorId -> do
      requestor <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
      case requestor.role of
        DP.FLEET_OWNER -> do
          -- fleetOwner is in req, should be the same as requestor
          whenJust req.fleetOwnerId \id -> do
            unless (id == requestorId) $ throwError AccessDenied
            DCommon.checkFleetOwnerVerification id merchant.fleetOwnerEnabledCheck
          process (processDriverByFleetOwner merchant merchantOpCity transporterConfig requestor)
        DP.OPERATOR ->
          -- fleetOwner is in csv row
          process (processDriverByOperator merchant merchantOpCity transporterConfig requestor)
        _ -> throwError AccessDenied

  pure $ Common.APISuccessWithUnprocessedEntities unprocessedEntities
  where
    processDriverByFleetOwner :: DM.Merchant -> DMOC.MerchantOperatingCity -> DTCConfig.TransporterConfig -> DP.Person -> DriverDetails -> Flow () -- TODO: create single query to update all later
    processDriverByFleetOwner merchant moc transporterConfig fleetOwner req_ = do
      validateDriverName req_.driverName
      let mobileCountryCode = P.getCountryMobileCode moc.country
      MobileValidation.validateMobileNumber transporterConfig req_.driverPhoneNumber mobileCountryCode moc.country
      whenJust req_.fleetPhoneNo $ \fleetPhoneNo -> MobileValidation.validateMobileNumber transporterConfig fleetPhoneNo mobileCountryCode moc.country
      whenJust req_.fleetPhoneNo \fleetPhoneNo -> do
        mobileNumberHash <- getDbHash fleetPhoneNo
        fleetOwnerCsv <-
          B.runInReplica $
            QP.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumberHash merchant.id DP.FLEET_OWNER
              >>= fromMaybeM (FleetOwnerNotFound fleetPhoneNo)
        unless (fleetOwnerCsv.id == fleetOwner.id) $
          throwError AccessDenied
      linkDriverToFleetOwner merchant moc fleetOwner Nothing req_

    processDriverByOperator :: DM.Merchant -> DMOC.MerchantOperatingCity -> DTCConfig.TransporterConfig -> DP.Person -> DriverDetails -> Flow () -- TODO: create single query to update all later
    processDriverByOperator merchant moc transporterConfig operator req_ = do
      validateDriverName req_.driverName
      let mobileCountryCode = P.getCountryMobileCode moc.country
      MobileValidation.validateMobileNumber transporterConfig req_.driverPhoneNumber mobileCountryCode moc.country
      whenJust req_.fleetPhoneNo $ \fleetPhoneNo -> MobileValidation.validateMobileNumber transporterConfig fleetPhoneNo mobileCountryCode moc.country
      case req_.fleetPhoneNo of
        Nothing -> do
          linkDriverToOperator merchant moc operator req_
        Just fleetPhoneNo -> do
          mobileNumberHash <- getDbHash fleetPhoneNo
          fleetOwner <-
            B.runInReplica $
              QP.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumberHash merchant.id DP.FLEET_OWNER
                >>= fromMaybeM (FleetOwnerNotFound fleetPhoneNo)
          DCommon.checkFleetOwnerVerification fleetOwner.id.getId merchant.fleetOwnerEnabledCheck
          association <-
            QFleetOperatorAssociation.findByFleetIdAndOperatorId fleetOwner.id.getId operator.id.getId True
              >>= fromMaybeM (InvalidRequest "FleetOperatorAssociation does not exist") -- TODO add error codes
          whenJust association.associatedTill $ \associatedTill -> do
            now <- getCurrentTime
            when (now > associatedTill) $
              throwError (InvalidRequest "FleetOperatorAssociation expired")
          linkDriverToFleetOwner merchant moc fleetOwner (Just operator.id) req_

    linkDriverToFleetOwner :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> Maybe (Id DP.Person) -> DriverDetails -> Flow () -- TODO: create single query to update all later
    linkDriverToFleetOwner merchant moc fleetOwner mbOperatorId req_ = do
      (person, isNew) <- fetchOrCreatePerson moc req_
      (if isNew then pure False else DDriver.checkFleetDriverAssociation fleetOwner.id person.id)
        >>= \isAssociated -> unless isAssociated $ do
          unless isNew $ SA.checkForDriverAssociationOverwrite merchant person.id
          fork "Sending Fleet Consent SMS to Driver" $ do
            let driverMobile = req_.driverPhoneNumber
            let onboardedOperatorId = if isNew then mbOperatorId else Nothing
            FDV.createFleetDriverAssociationIfNotExists person.id fleetOwner.id onboardedOperatorId (fromMaybe DVC.CAR req_.driverOnboardingVehicleCategory) False Nothing
            whenJust req_.badgeType $ createOrUpdateFleetBadge merchant moc person req_ fleetOwner
            sendDeepLinkForAuth person driverMobile moc.merchantId moc.id moc.country fleetOwner

    linkDriverToOperator :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> DriverDetails -> Flow () -- TODO: create single query to update all later
    linkDriverToOperator merchant moc operator req_ = do
      (person, isNew) <- fetchOrCreatePerson moc req_
      (if isNew then pure False else DDriver.checkDriverOperatorAssociation person.id operator.id)
        >>= \isAssociated -> unless isAssociated $ do
          unless isNew $ SA.checkForDriverAssociationOverwrite merchant person.id
          fork "Sending Operator Consent SMS to Driver" $ do
            let driverMobile = req_.driverPhoneNumber
            QDOA.createDriverOperatorAssociationIfNotExists moc person.id operator.id (fromMaybe DVC.CAR req_.driverOnboardingVehicleCategory) False
            sendOperatorDeepLinkForAuth person driverMobile moc.merchantId moc.id moc.country operator

    sendDeepLinkForAuth :: DP.Person -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.Country -> DP.Person -> Flow ()
    sendDeepLinkForAuth person mobileNumber merchantId merchantOpCityId country fleetOwner = do
      let countryCode = fromMaybe (P.getCountryMobileCode country) person.mobileCountryCode
          phoneNumber = countryCode <> mobileNumber
      smsCfg <- asks (.smsCfg)
      withLogTag ("sending Deeplink Auth SMS" <> getId person.id) $ do
        (mbSender, message, templateId) <-
          MessageBuilder.buildFleetDeepLinkAuthMessage merchantOpCityId $
            MessageBuilder.BuildFleetDeepLinkAuthMessage
              { fleetOwnerName = fleetOwner.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult

    sendOperatorDeepLinkForAuth :: DP.Person -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.Country -> DP.Person -> Flow ()
    sendOperatorDeepLinkForAuth person mobileNumber merchantId merchantOpCityId country operator = do
      let countryCode = fromMaybe (P.getCountryMobileCode country) person.mobileCountryCode
          phoneNumber = countryCode <> mobileNumber
      smsCfg <- asks (.smsCfg)
      withLogTag ("sending Operator Deeplink Auth SMS" <> getId person.id) $ do
        (mbSender, message, templateId) <-
          MessageBuilder.buildOperatorDeepLinkAuthMessage merchantOpCityId $
            MessageBuilder.BuildOperatorDeepLinkAuthMessage
              { operatorName = operator.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult

validateDriverName :: Text -> Flow ()
validateDriverName driverName = do
  result <- try (void $ runRequestValidation Common.validateUpdateDriverNameReq (Common.UpdateDriverNameReq {firstName = driverName, middleName = Nothing, lastName = Nothing}))
  case result of
    Left (_ :: SomeException) -> throwError $ InvalidRequest "Driver name should not contain numbers and should have at least 1 letter and at most 50 letters"
    Right _ -> pure ()

parseDriverInfo :: Int -> CreateDriversCSVRow -> Flow DriverDetails
parseDriverInfo idx row = do
  driverName <- Csv.cleanCSVField idx row.driverName "Driver name"
  driverPhoneNumber <- Csv.cleanCSVField idx row.driverPhoneNumber "Mobile number"
  let driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory = Csv.readMaybeCSVField idx (fromMaybe "" row.driverOnboardingVehicleCategory) "Onboarding Vehicle Category"
      fleetPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.fleetPhoneNo) "Fleet number"
      badgeName :: Maybe Text = Csv.readMaybeCSVField idx (fromMaybe "" row.badgeName) "Badge name"
      badgeRank :: Maybe Text = Csv.readMaybeCSVField idx (fromMaybe "" row.badgeRank) "Badge rank"
      badgeType :: Maybe DFBT.FleetBadgeType = Csv.readMaybeCSVField idx (fromMaybe "" row.badgeType) "Badge type"
  pure $ DriverDetails driverName driverPhoneNumber driverOnboardingVehicleCategory fleetPhoneNo badgeName badgeRank badgeType

fetchOrCreatePerson :: DMOC.MerchantOperatingCity -> DriverDetails -> Flow (DP.Person, Bool)
fetchOrCreatePerson moc req_ = do
  let mobileCountryCode = P.getCountryMobileCode moc.country
  let authData =
        DReg.AuthReq
          { mobileNumber = Just req_.driverPhoneNumber,
            mobileCountryCode = Just mobileCountryCode,
            merchantId = moc.merchantId.getId,
            merchantOperatingCity = Just moc.city,
            email = Nothing,
            name = Just req_.driverName,
            identifierType = Just DP.MOBILENUMBER,
            registrationLat = Nothing,
            registrationLon = Nothing,
            otpChannel = Nothing
          }
  mobileNumberHash <- getDbHash req_.driverPhoneNumber
  QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumberHash moc.merchantId DP.DRIVER
    >>= \case
      Nothing -> do
        cloudType <- asks (.cloudType)
        person <- DReg.createDriverWithDetails authData Nothing Nothing Nothing Nothing Nothing Nothing cloudType moc.merchantId moc.id True
        let isNew = True in pure (person, isNew)
      Just person -> do
        QPerson.updateName req_.driverName person.id
        let isNew = False in pure (person {DP.firstName = req_.driverName}, isNew)

---------------------------------------------------------------------
postDriverDashboardFleetTrackDriver :: ShortId DM.Merchant -> Context.City -> Text -> Common.TrackDriverLocationsReq -> Flow Common.TrackDriverLocationsRes
postDriverDashboardFleetTrackDriver _ _ fleetOwnerId req = do
  void $
    mapM
      ( \driverId ->
          WMB.checkOnboardingCategoryForFleetDriverAssociation (Id fleetOwnerId) (cast driverId) >>= \isAssociated -> unless isAssociated (throwError $ DriverNotLinkedToFleet driverId.getId)
      )
      req.driverIds
  driverLocations <- LF.driversLocation (map (cast @Common.Driver @DP.Person) req.driverIds)
  return $ Common.TrackDriverLocationsRes $ map buildDriverLocationsRes driverLocations
  where
    buildDriverLocationsRes DDL.DriverLocation {..} =
      Common.TrackDriverLocation
        { driverId = cast driverId,
          point = LatLong {..},
          lastUpdatedAt = coordinatesCalculatedAt
        }

getDriverFleetWmbRouteDetails :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow Common.RouteDetails
getDriverFleetWmbRouteDetails _ _ _ routeCode = WMB.getRouteDetails routeCode

postDriverFleetGetNearbyDrivers :: ShortId DM.Merchant -> Context.City -> Text -> Common.NearbyDriverReq -> Flow Common.NearbyDriverResp
postDriverFleetGetNearbyDrivers merchantShortId _ fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  let vehicleVariantList = Just $ maybe [DV.BUS_NON_AC, DV.BUS_AC] (map DCommon.castDashboardVehicleVariantToDomain) (req.vehicleVariantList)
  nearbyDriverLocations <- LF.nearBy req.point.lat req.point.lon Nothing vehicleVariantList req.radius merchant.id (Just fleetOwnerId) Nothing
  return $
    Common.NearbyDriverResp $
      catMaybes $
        map
          ( \driverLocation ->
              case (driverLocation.rideDetails >>= (.rideInfo), driverLocation.rideDetails <&> (.rideStatus)) of
                (Just (LTST.Bus LTST.BusRideInfo {..}), Just rideStatus) ->
                  case driverName of
                    Just driverName' ->
                      Just $
                        Common.DriverInfo
                          { driverId = cast driverLocation.driverId,
                            driverName = driverName',
                            vehicleNumber = busNumber,
                            rideStatus = mkRideStatus rideStatus,
                            point = LatLong {lat = driverLocation.lat, lon = driverLocation.lon},
                            ..
                          }
                    _ -> Nothing
                _ -> Nothing
          )
          nearbyDriverLocations
  where
    mkRideStatus = \case
      TR.INPROGRESS -> Common.ON_RIDE
      _ -> Common.ON_PICKUP

getDriverFleetAccessList :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Flow Common.FleetOwnerListRes
getDriverFleetAccessList _ _ mbFleetMemberId = do
  fleetMemberId <- mbFleetMemberId & fromMaybeM (InvalidRequest "Fleet member ID is required")
  fleetOwners <- B.runInMasterDbAndRedis $ FMA.findAllByfleetMemberId fleetMemberId
  allFleetGroups <- B.runInMasterDbAndRedis $ FMA.findAllWithOwnerIds ((.fleetOwnerId) <$> fleetOwners)
  ownersList <-
    mapM
      ( \fleetMemberAssociation -> do
          fleetMemberAssociation' <- FMA.findOneByFleetOwnerId fleetMemberAssociation.fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetOwnerId)
          person <- QP.findById (Id fleetMemberAssociation'.fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation'.fleetOwnerId)
          let groups = map (\fma -> Common.FleetGroup {Common.groupCode = fromMaybe mempty fma.groupCode, Common.level = fromMaybe (-1) fma.level, parentGroupCode = Nothing}) $ filter (\fma -> fma.fleetOwnerId == person.id.getId) allFleetGroups
          return $
            Common.FleetOwnerListAPIEntity
              { fleetOwnerId = fleetMemberAssociation.fleetOwnerId,
                fleetOwnerName = person.firstName <> maybe "" (" " <>) person.lastName,
                fleetGroup =
                  ((,) <$> fleetMemberAssociation.level <*> fleetMemberAssociation.groupCode)
                    <&> \case
                      (level, groupCode) ->
                        Common.FleetGroup {level, parentGroupCode = fleetMemberAssociation.parentGroupCode, groupCode},
                order = fleetMemberAssociation.order,
                enabled = fleetMemberAssociation.enabled,
                ..
              }
      )
      fleetOwners
  return $ Common.FleetOwnerListRes {..}

postDriverFleetAccessSelect :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetAccessSelect _ _ fleetOwnerId mbFleetMemberId mbOnlySingle enable = do
  fleetMemberId <- mbFleetMemberId & fromMaybeM (InvalidRequest "Fleet member ID is required")
  let onlySingle = fromMaybe False mbOnlySingle
  when (onlySingle && enable) $ do
    fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
    FMA.updateFleetMembersActiveStatus False fleetMemberId (map fst fleetOwnerIds)
    clearFleetMemberCacheMultiple fleetMemberId (map fst fleetOwnerIds)
  FMA.updateFleetMemberActiveStatus enable fleetMemberId fleetOwnerId
  clearFleetMemberCache fleetMemberId (Just fleetOwnerId)
  return Success

postDriverFleetV2AccessSelect :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetV2AccessSelect _ _ mbFleetMemberId mbFleetOwnerId mbGroupCode mbOnlyCurrent enable = do
  fleetMemberId <- mbFleetMemberId & fromMaybeM (InvalidRequest "Fleet member ID is required")
  let onlyCurrent = fromMaybe False mbOnlyCurrent
  when (onlyCurrent && enable) $ do
    fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
    FMA.updateFleetMembersActiveStatus False fleetMemberId (map fst fleetOwnerIds)
    clearFleetMemberCacheMultiple fleetMemberId (map fst fleetOwnerIds)
  case (mbFleetOwnerId, mbGroupCode) of
    (Just fleetOwnerId, _) -> do
      FMA.updateFleetMemberActiveStatus enable fleetMemberId fleetOwnerId
      clearFleetMemberCache fleetMemberId (Just fleetOwnerId)
    (Nothing, Just groupCode) -> do
      -- TODO: add group code support
      FMA.updateFleetMembersActiveStatusByGroupCode enable fleetMemberId (Just groupCode)
      clearFleetMemberCache fleetMemberId Nothing
    _ -> return ()
  return Success

postDriverFleetV2AccessMultiOwnerIdSelect :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Bool -> Bool -> Common.MultiOwnerSelect -> Flow APISuccess
postDriverFleetV2AccessMultiOwnerIdSelect _ _ mbFleetMemberId mbOnlyCurrent enable req = do
  fleetMemberId <- mbFleetMemberId & fromMaybeM (InvalidRequest "Fleet member ID is required")
  let onlyCurrent = fromMaybe False mbOnlyCurrent
  when (onlyCurrent && enable) $ do
    fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
    FMA.updateFleetMembersActiveStatus False fleetMemberId (map fst fleetOwnerIds)
    clearFleetMemberCacheMultiple fleetMemberId (map fst fleetOwnerIds)
  FMA.updateFleetMembersActiveStatus enable fleetMemberId req.fleetOwnerIds
  clearFleetMemberCacheMultiple fleetMemberId req.fleetOwnerIds
  return Success

clearFleetMemberCache :: Text -> Maybe Text -> Flow ()
clearFleetMemberCache fleetMemberId mbFleetOwnerId = mapM_ Redis.del [key1, key2]
  where
    key1 = "CachedFleetOwnerId:memberPersonId-" <> fleetMemberId <> "-fleetOwnerId-" <> fromMaybe "None" mbFleetOwnerId
    key2 = "CachedFleetOwnerIds:memberPersonId-" <> fleetMemberId <> "-fleetOwnerId-" <> fromMaybe "None" mbFleetOwnerId

clearFleetMemberCacheMultiple :: Text -> [Text] -> Flow ()
clearFleetMemberCacheMultiple fleetMemberId fleetOwnerIds = mapM_ (clearFleetMemberCache fleetMemberId . Just) fleetOwnerIds

-- Helper function to convert RegisterRCReq to AddVehicleReq
convertToAddVehicleReq :: Common.RegisterRCReq -> Common.AddVehicleReq
convertToAddVehicleReq rcReq =
  Common.AddVehicleReq
    { registrationNo = rcReq.vehicleRegistrationCertNumber,
      vehicleClass = "",
      capacity = Nothing,
      colour = maybe "" (.vehicleColour) rcReq.vehicleDetails,
      energyType = Nothing,
      model = maybe "" (.vehicleModel) rcReq.vehicleDetails,
      make = maybe "" (.vehicleManufacturer) rcReq.vehicleDetails,
      airConditioned = rcReq.airConditioned,
      driverName = Nothing,
      imageId = Just rcReq.imageId,
      vehicleCategory = rcReq.vehicleCategory,
      oxygen = rcReq.oxygen,
      ventilator = rcReq.ventilator,
      dateOfRegistration = rcReq.dateOfRegistration,
      mYManufacturing = Nothing,
      vehicleModelYear = Nothing,
      vehicleTags = Nothing,
      fuelType = Nothing
    }

getDriverDashboardInternalHelperGetFleetOwnerId :: (ShortId DM.Merchant -> Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow Text)
getDriverDashboardInternalHelperGetFleetOwnerId _ _ mbFleetOwnerId memberPersonId = getFleetOwnerId memberPersonId mbFleetOwnerId

getDriverDashboardInternalHelperGetFleetOwnerIds :: (ShortId DM.Merchant -> Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow [(Text, Text)])
getDriverDashboardInternalHelperGetFleetOwnerIds _ _ mbFleetOwnerId memberPersonId = getFleetOwnerIds memberPersonId mbFleetOwnerId

-------------------------------------- multiple fleet queries ------------------------------------------------------

getListOfVehiclesMultiFleet :: Maybe Text -> [Text] -> Maybe Int -> Maybe Int -> Maybe Common.FleetVehicleStatus -> Id DM.Merchant -> Maybe Text -> Maybe Text -> Flow [DVRC.VehicleRegistrationCertificate]
getListOfVehiclesMultiFleet mbVehicleNo fleetOwnerIds mbLimit mbOffset mbStatus merchantId mbSearchString statusAwareVehicleNo = do
  let limit = fromIntegral $ min 10 $ fromMaybe 5 mbLimit
      offset = fromIntegral $ fromMaybe 0 mbOffset
  case mbVehicleNo of
    Just vehicleNo -> RCQuery.partialFindLastVehicleRCFleetMF vehicleNo fleetOwnerIds limit offset
    Nothing -> do
      case mbStatus of
        -- This Status is Associated with Driver and
        Just Common.Active -> RCQuery.findAllActiveRCForFleetByLimitOffsetMF fleetOwnerIds merchantId limit offset mbSearchString statusAwareVehicleNo
        Just Common.InActive -> RCQuery.findAllInactiveRCForFleetMF fleetOwnerIds limit offset merchantId statusAwareVehicleNo
        -- This Status is only Associated purely with RCs and Not Associated with any Driver

        -- make changes here for onride and tripassigned
        Just Common.OnRide -> RCQuery.findAllVehicleByStatusForFleetByLimitOffsetMF fleetOwnerIds merchantId limit offset mbSearchString statusAwareVehicleNo DTT.IN_PROGRESS
        Just Common.TripAssigned -> RCQuery.findAllVehicleByStatusForFleetByLimitOffsetMF fleetOwnerIds merchantId limit offset mbSearchString statusAwareVehicleNo DTT.TRIP_ASSIGNED
        Just Common.Valid -> RCQuery.findAllRCByStatusForFleetMF fleetOwnerIds (Just $ castFleetVehicleStatus mbStatus) limit offset merchantId statusAwareVehicleNo
        Just Common.Invalid -> RCQuery.findAllRCByStatusForFleetMF fleetOwnerIds (Just $ castFleetVehicleStatus mbStatus) limit offset merchantId statusAwareVehicleNo
        Just Common.Pending -> RCQuery.findAllRCByStatusForFleetMF fleetOwnerIds (Just $ castFleetVehicleStatus mbStatus) limit offset merchantId statusAwareVehicleNo
        Nothing -> RCQuery.findAllRCByStatusForFleetMF fleetOwnerIds Nothing limit offset merchantId statusAwareVehicleNo

getListOfDriversMultiFleet :: Maybe Text -> Maybe Text -> [Text] -> Id DM.Merchant -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Common.DriverMode -> Maybe Text -> Maybe Text -> Maybe Bool -> Flow ([FleetDriverAssociation], [DP.Person], [DI.DriverInformation])
getListOfDriversMultiFleet _ mbDriverPhNo fleetOwnerIds _ mbIsActive mbLimit mbOffset mbMode mbName mbSearchString mbHasRequestReason = do
  let limit = min 10 $ fromMaybe 5 mbLimit
      offset = fromMaybe 0 mbOffset

  mobileNumberHash <- case mbSearchString of
    Just _ -> pure Nothing
    Nothing -> case mbDriverPhNo of
      Just phNo -> Just <$> getDbHash phNo
      Nothing -> pure Nothing

  let mode = castDashboardDriverStatus <$> mbMode
  driverAssociationAndInfo <- FDV.findAllActiveDriverByFleetOwnerIdWithDriverInfoMF fleetOwnerIds limit offset mobileNumberHash mbName mbSearchString mbIsActive mode mbHasRequestReason
  let (fleetDriverAssociation, person, driverInformation) = unzip3 driverAssociationAndInfo
  return (fleetDriverAssociation, person, driverInformation)

---------------------------------------------------------------------
getDriverFleetBookings ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Flow Common.FleetBookingsInformationResponse
getDriverFleetBookings _ _ memberPersonId mbLimit mbOffset mbFrom mbTo mbStatus vehicleNo mbSearchByFleetOwnerId mbSearchByTicketPlaceId = do
  let searchByFleetOwnerId = fromMaybe True mbSearchByFleetOwnerId
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId Nothing
  let fleetOwnerIds = map fst fleetOwnerInfo
      fleetNameMap = Map.fromList fleetOwnerInfo
  mbFleetOwnerInfo <-
    if fromMaybe False mbSearchByTicketPlaceId
      then join <$> traverse (FOI.findByPrimaryKey . Id) (headMay fleetOwnerIds)
      else pure Nothing

  ticketBookings <- QFBI.findFleetBookingInformationByFleetOwnerIdsAndFilters fleetOwnerIds mbFrom mbTo mbLimit mbOffset searchByFleetOwnerId (mbFleetOwnerInfo >>= FOI.ticketPlaceId) vehicleNo mbStatus

  ticketBookingsList <- forM ticketBookings $ \booking -> do
    let fleetOwnerId = fromMaybe "" (fmap (.getId) booking.fleetOwnerId)
        fleetOwnerName = fromMaybe "" (Map.lookup fleetOwnerId fleetNameMap)
    decryptMobileNumber <- mapM decrypt booking.customerMobileNumber
    pure $
      Common.FleetBookingItem
        { bookingId = booking.bookingId,
          serviceId = fromMaybe "" booking.serviceId,
          serviceName = fromMaybe "" booking.serviceName,
          ticketBookingShortId = booking.ticketBookingShortId,
          ticketBookingServiceShortId = booking.ticketBookingServiceShortId,
          placeName = fromMaybe "" booking.placeName,
          vehicleNo = fromMaybe "" booking.vehicleNo,
          amount = fromMaybe 0.0 (fmap realToFrac booking.amount),
          bookedSeats = fromMaybe 0 booking.bookedSeats,
          status = fromMaybe "" booking.status,
          paymentMethod = booking.paymentMethod,
          visitDate = booking.visitDate,
          createdAt = booking.createdAt,
          updatedAt = booking.updatedAt,
          fleetOwnerId = fleetOwnerId,
          fleetOwnerName = fleetOwnerName,
          customerMobileNumber = decryptMobileNumber,
          customerName = booking.customerName
        }

  let summary = Common.Summary {totalCount = 10000, count = length ticketBookingsList}
  pure $
    Common.FleetBookingsInformationResponse
      { bookings = ticketBookingsList,
        summary = summary
      }

---------------------------------------------------------------------
getDriverFleetAssignments ::
  ( ShortId DM.Merchant ->
    Context.City ->
    Text ->
    Maybe Int ->
    Maybe Int ->
    Maybe UTCTime ->
    Maybe UTCTime ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Environment.Flow Common.FleetBookingAssignmentsResponse
  )
getDriverFleetAssignments _ _ memberPersonId mbLimit mbOffset mbFrom mbTo mbVehicleNo mbMainAssignmentId mbBookingId = do
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId Nothing
  let fleetOwnerIds = map fst fleetOwnerInfo
      fleetNameMap = Map.fromList fleetOwnerInfo
  ticketAssignments <- QFBA.findFleetBookingAssignmentsByFleetOwnerIdsAndFilters fleetOwnerIds mbMainAssignmentId mbFrom mbTo mbLimit mbOffset mbVehicleNo mbBookingId

  ticketAssignmentsList <- forM ticketAssignments $ \assignment -> do
    let fleetOwnerId = assignment.fleetOwnerId
        fleetOwnerName = fromMaybe "" (Map.lookup fleetOwnerId fleetNameMap)

    pure $
      Common.FleetBookingAssignmentItem
        { bookingId = assignment.bookingId,
          serviceId = fromMaybe "" assignment.serviceId,
          serviceName = fromMaybe "" assignment.serviceName,
          placeName = fromMaybe "" assignment.placeName,
          vehicleNo = assignment.vehicleNo,
          amount = fromMaybe 0.0 (fmap realToFrac assignment.amount),
          visitDate = assignment.visitDate,
          createdAt = assignment.createdAt,
          fleetOwnerId = fleetOwnerId,
          fleetOwnerName = fleetOwnerName,
          paymentMethod = assignment.paymentMethod
        }

  let summary = Common.Summary {totalCount = 10000, count = length ticketAssignmentsList}
  pure $ Common.FleetBookingAssignmentsResponse {bookings = ticketAssignmentsList, summary = summary}

---------------------------------------------------------------------
postDriverFleetLocationList ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.DriverLocationListReq ->
  Environment.Flow Common.DriverLocationListResp
postDriverFleetLocationList _ _ memberPersonId req = do
  if null req.driverIds
    then throwError $ InvalidRequest "Driver IDs are required"
    else do
      rateLimitOptions <- asks (.driverFleetLocationListAPIRateLimitOptions)
      checkSlidingWindowLimitWithOptions postDriverFleetLocationListHitsCountKey rateLimitOptions
      currentTime <- getCurrentTime
      let firstDate = fromMaybe (negate 86400 `addUTCTime` currentTime) req.firstDate
          lastDate = fromMaybe currentTime req.lastDate
      allLocations <- fmap concat $
        forM req.driverIds $ \driverId -> do
          let personId = cast driverId
          driverEdaKafkaList <- CHDriverEda.findAllTupleByTimestamp firstDate lastDate personId $ min 10000 $ fromMaybe 1000 req.limit
          pure $
            map
              ( \(mbLat, mbLon, timestamp, mbAccuracy, _mbRideStatus) ->
                  Common.DriverLocation
                    { driverId = driverId,
                      lat = mbLat,
                      lon = mbLon,
                      timeStamp = timestamp,
                      accuracy = mbAccuracy
                    }
              )
              driverEdaKafkaList
      pure $ Common.DriverLocationListResp allLocations
  where
    postDriverFleetLocationListHitsCountKey = "BPP:API:ACTION:DriverFleetLocationList:PersonId:" <> memberPersonId <> ":hitsCount"

createOrUpdateFleetBadge :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> DriverDetails -> DP.Person -> DFBT.FleetBadgeType -> Flow ()
createOrUpdateFleetBadge merchant merchantOpCity person driverDetails fleetOwner badgeType = do
  now <- getCurrentTime
  let badgeName = fromMaybe driverDetails.driverName driverDetails.badgeName
  mbBadgeAssociation <- CFBA.findOneFleetBadgeAssociationByFleetOwnerIdAndDriverIdAndBadgeTypeAndIsActive fleetOwner.id.getId person.id badgeType True
  case mbBadgeAssociation of
    Just badgeAssociation -> do
      QFB.updateBadgeNameAndRankById badgeName driverDetails.badgeRank badgeAssociation.badgeId
    Nothing -> do
      fleetBadgeId <- generateGUID
      let newBadge = buildNewBadge fleetBadgeId now
      QFB.create newBadge
      WMB.linkFleetBadge (cast person.id) merchant.id merchantOpCity.id (fleetOwner.id.getId) newBadge badgeType
  where
    buildNewBadge badgeId now =
      DFB.FleetBadge
        { badgeName = fromMaybe driverDetails.driverName driverDetails.badgeName,
          badgeType = badgeType,
          createdAt = now,
          personId = Just person.id,
          fleetOwnerId = fleetOwner.id,
          id = Id badgeId,
          merchantId = merchant.id,
          merchantOperatingCityId = merchantOpCity.id,
          updatedAt = now,
          badgeRank = driverDetails.badgeRank
        }

---------------------------------------------------------------------
postDriverFleetGetDriverDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.DriverDetailsReq ->
  Environment.Flow Common.DriverDetailsResp
postDriverFleetGetDriverDetails _ _ _fleetOwnerId req = do
  respArray <-
    for req.driverIds $ \driverId -> do
      let personId = cast @Common.Driver @DP.Person driverId
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      mbVehicle <- B.runInReplica $ QVehicle.findById person.id
      decryptedMobileNumber <- mapM decrypt person.mobileNumber
      pure $
        Common.DriverDetails
          { driverId = cast person.id,
            firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName,
            mobileCountryCode = person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            email = person.email,
            vehicleNumber = mbVehicle <&> (.registrationNo),
            vehicleVariant = DCommon.castVehicleVariantDashboard $ mbVehicle <&> (.variant)
          }
  pure $ Common.DriverDetailsResp respArray

---------------------------------------------------------------------
postDriverFleetGetNearbyDriversV2 ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.NearbyDriversReqV2 ->
  Environment.Flow Common.NearbyDriversRespV2
postDriverFleetGetNearbyDriversV2 merchantShortId _ fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  nearbyDriverLocations <- LF.nearBy req.point.lat req.point.lon Nothing (Just $ map DCommon.castDashboardVehicleVariantToDomain req.vehicleVariantList) req.radius merchant.id (Just fleetOwnerId) Nothing
  return $
    Common.NearbyDriversRespV2 $
      mapMaybe (filterByVehicleType req.vehicleVariantList) nearbyDriverLocations
  where
    filterByVehicleType vlist driverLocation
      | isBusRequest = handleBusDriver driverLocation
      | isVipRequest = handleVipDriver driverLocation
      | otherwise = Nothing
      where
        isBusRequest = all (`elem` Set.fromList [DV.BUS_AC, DV.BUS_NON_AC]) vlist
        isVipRequest = all (`elem` Set.fromList [DV.VIP_OFFICER, DV.VIP_ESCORT]) vlist

    handleBusDriver driverLocation =
      case (driverLocation.rideDetails >>= (.rideInfo), driverLocation.rideDetails <&> (.rideStatus)) of
        (Just (LTST.Bus LTST.BusRideInfo {..}), Just rideStatus) ->
          Just $
            Common.NearbyDriverDetails
              { driverId = cast driverLocation.driverId,
                rideInfo = Just $ Common.Bus Common.BusRideInfo {..},
                rideStatus = Just $ mkRideStatus rideStatus,
                rideId = driverLocation.rideDetails <&> (.rideId),
                point = LatLong {lat = driverLocation.lat, lon = driverLocation.lon}
              }
        _ -> Nothing

    handleVipDriver driverLocation =
      case (driverLocation.rideDetails >>= (.rideInfo), driverLocation.rideDetails <&> (.rideStatus)) of
        (Just (LTST.Pilot LTST.PilotRideInfo {..}), Just rideStatus) ->
          Just $
            Common.NearbyDriverDetails
              { driverId = cast driverLocation.driverId,
                rideInfo = Just $ Common.Pilot Common.PilotRideInfo {..},
                rideStatus = Just $ mkRideStatus rideStatus,
                rideId = driverLocation.rideDetails <&> (.rideId),
                point = LatLong {lat = driverLocation.lat, lon = driverLocation.lon}
              }
        _ ->
          Just $
            Common.NearbyDriverDetails
              { driverId = cast driverLocation.driverId,
                rideInfo = Nothing,
                rideStatus = Nothing,
                rideId = Nothing,
                point = LatLong {lat = driverLocation.lat, lon = driverLocation.lon}
              }

    mkRideStatus = \case
      TR.INPROGRESS -> Common.ON_RIDE
      _ -> Common.ON_PICKUP

---------------------------------------------------------------------

getDriverFleetDashboardAnalyticsAllTime :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.AllTimeFleetAnalyticsRes
getDriverFleetDashboardAnalyticsAllTime merchantShortId opCity fleetOwnerId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  -- Redis keys for fleet aggregates
  let allTimeKeysData = Analytics.fleetAllTimeKeys fleetOwnerId
  logTagInfo "fleetAllTimeKeysData" (show allTimeKeysData)
  -- try redis first
  mbAllTimeKeysRes <- mapM (Redis.get @Int) allTimeKeysData
  logTagInfo "fleetMbAllTimeKeysRes" (show mbAllTimeKeysRes)

  onlineDriverCount <- DDF.getOnlineKeyValue fleetOwnerId
  logTagInfo "onlineDriverCount" (show onlineDriverCount)
  -- fallback to ClickHouse and populate cache when missing
  (activeDriverCount, activeVehicleCount, currentOnlineDriverCount) <- do
    if all isJust mbAllTimeKeysRes && isJust onlineDriverCount
      then do
        let res = Analytics.convertToFleetAllTimeFallbackRes (Analytics.zipJusts Analytics.fleetAllTimeMetrics mbAllTimeKeysRes) onlineDriverCount
        logTagInfo "FleetAllTimeFallbackRes" (show res)
        Analytics.extractFleetAnalyticsData res
      else do
        res <- Analytics.handleCacheMissForAnalyticsAllTimeCommon DP.FLEET_OWNER fleetOwnerId allTimeKeysData
        logTagInfo "FleetAllTimeFallbackRes" (show res)
        Analytics.extractFleetAnalyticsData res
  -- compute remaining metrics from DB
  mfos <- QFleetOperatorStats.findByPrimaryKey fleetOwnerId
  let completedRides = mfos >>= (.totalCompletedRides)
      ratingSum = mfos >>= (.totalRatingScore)
      ratingCount = mfos >>= (.totalRatingCount)
      averageDriverRatings =
        ratingSum >>= \rs ->
          ratingCount >>= \rc ->
            if rc > 0 then Just (realToFrac rs / realToFrac rc) else Nothing
  pure $
    Common.AllTimeFleetAnalyticsRes
      { activeVehicle = activeVehicleCount,
        completedRides = completedRides,
        totalActiveDrivers = activeDriverCount,
        currentOnlineDrivers = currentOnlineDriverCount,
        averageDriverRatings = averageDriverRatings
      }

getDriverFleetDashboardAnalytics :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Common.FleetAnalyticsResponseType -> Data.Time.Day -> Data.Time.Day -> Flow Common.FleetAnalyticsRes
getDriverFleetDashboardAnalytics merchantShortId opCity fleetOwnerId mbResponseType fromDay toDay = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  let useDBForAnalytics = transporterConfig.analyticsConfig.useDbForEarningAndMetrics
  case mbResponseType of
    Just Common.EARNINGS -> do
      earningsAgg <-
        if useDBForAnalytics
          then QFODSExtra.sumFleetEarningsByFleetOwnerIdAndDateRangeDB fleetOwnerId fromDay toDay
          else CFODS.sumFleetEarningsByFleetOwnerIdAndDateRange fleetOwnerId fromDay toDay
      let grossEarningsAmount = earningsAgg.totalEarningSum
          cashPlatformFeesAmount = earningsAgg.cashPlatformFeesSum
          onlinePlatformFeesAmount = earningsAgg.onlinePlatformFeesSum
          totalPlatformFeesAmount = liftA2 (+) cashPlatformFeesAmount onlinePlatformFeesAmount
          onlineDurationInHours :: Maybe Rational
          onlineDurationInHours =
            earningsAgg.onlineDurationSum >>= \(Seconds sec) ->
              let hours = fromIntegral sec / 3600
               in if hours > 0 then Just hours else Nothing
          netEarningsAmount = liftA2 (-) grossEarningsAmount totalPlatformFeesAmount
          grossEarningsPerHourAmount =
            grossEarningsAmount >>= \gross ->
              onlineDurationInHours <&> \hours -> gross / HighPrecMoney hours
          netEarningsPerHourAmount =
            netEarningsAmount >>= \net ->
              onlineDurationInHours <&> \hours -> net / HighPrecMoney hours

      pure $
        Common.Earnings $
          Common.EarningFleetAnalyticsRes
            { grossEarnings = fmap (`PriceAPIEntity` transporterConfig.currency) grossEarningsAmount,
              platformFees = fmap (`PriceAPIEntity` transporterConfig.currency) totalPlatformFeesAmount,
              netEarnings = fmap (`PriceAPIEntity` transporterConfig.currency) netEarningsAmount,
              grossEarningsPerHour = fmap (`PriceAPIEntity` transporterConfig.currency) grossEarningsPerHourAmount,
              netEarningsPerHour = fmap (`PriceAPIEntity` transporterConfig.currency) netEarningsPerHourAmount
            }
    _ -> do
      agg <-
        if useDBForAnalytics
          then QFODSExtra.sumFleetMetricsByFleetOwnerIdAndDateRangeDB fleetOwnerId fromDay toDay
          else CFODS.sumFleetMetricsByFleetOwnerIdAndDateRange fleetOwnerId fromDay toDay
      let totalEarnings = fmap (`PriceAPIEntity` transporterConfig.currency) agg.totalEarningSum
          totalDistance = agg.totalDistanceSum
          completedRides = agg.totalCompletedRidesSum
          totalRideRequest = agg.totalRequestCountSum
          acceptedRequest = agg.acceptationRequestCountSum
          rejectedRequest = agg.rejectedRequestCountSum
          pulledRequest = agg.pulledRequestCountSum
          driverCancelled = agg.driverCancellationCountSum
          customerCancelled = agg.customerCancellationCountSum
      pure $
        Common.Filtered $
          Common.FilteredFleetAnalyticsRes {..}

getDriverDashboardFleetTripWaypoints ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.TripTransaction ->
  Text ->
  Maybe Bool ->
  Maybe Text ->
  Flow Common.TripTransactionWaypointsRes
getDriverDashboardFleetTripWaypoints _ _ tripTransactionId fleetOwnerId mbliteMode memberPersonId = do
  let liteMode = fromMaybe True mbliteMode
  case liteMode of
    True -> do
      tripTransaction <- QTT.findByTransactionId (cast tripTransactionId) >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
      pure $ Common.TripTransactionWaypointsRes {waypoints = [], estimatedRouteDetails = tripTransaction.tripEstimatedRouteDetails >>= (\estimatedRoute -> Just (Common.EstimatedRouteDetails {distance = estimatedRoute.distance, duration = estimatedRoute.duration, polyline = estimatedRoute.polyline}))}
    False -> do
      rateLimitOptions <- asks (.driverFleetLocationListAPIRateLimitOptions)
      checkSlidingWindowLimitWithOptions (driverFleetTripWaypointsHitsCountKey fleetOwnerId memberPersonId) rateLimitOptions
      tripTransaction <- QTT.findByTransactionId (cast tripTransactionId) >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
      unless (tripTransaction.status == COMPLETED) $ throwError (InvalidRequest "Trip transaction is not completed")
      startTime <- maybe (throwError (InvalidRequest "Trip transaction start time not found")) pure tripTransaction.tripStartTime
      endTime <- maybe (throwError (InvalidRequest "Trip transaction end time not found")) pure tripTransaction.tripEndTime
      driverEdaKafkaList <- CHDriverEda.findAllTuple startTime endTime tripTransaction.driverId (Just $ cast tripTransaction.id)
      actualRoute <- mapM getActualRouteFromTuple driverEdaKafkaList
      pure $ Common.TripTransactionWaypointsRes {waypoints = actualRoute, estimatedRouteDetails = tripTransaction.tripEstimatedRouteDetails >>= (\estimatedRoute -> Just (Common.EstimatedRouteDetails {distance = estimatedRoute.distance, duration = estimatedRoute.duration, polyline = estimatedRoute.polyline}))}
  where
    getActualRouteFromTuple :: (Maybe Double, Maybe Double, UTCTime, Maybe Double, Maybe CHDriverEda.Status) -> Flow Common.ActualRoute
    getActualRouteFromTuple (mbLat, mbLon, timestamp, mbAccuracy, _) =
      case (mbLat, mbLon) of
        (Just lat', Just lon') -> pure Common.ActualRoute {lat = lat', lon = lon', timestamp = timestamp, accuracy = mbAccuracy}
        _ -> throwError $ InvalidRequest "Couldn't find driver's location."

driverFleetTripWaypointsHitsCountKey :: Text -> Maybe Text -> Text
driverFleetTripWaypointsHitsCountKey fleetOwnerId memberPersonId = "driverFleetTripWaypointsHitsCount:" <> fleetOwnerId <> ":" <> fromMaybe "" memberPersonId

postDriverFleetDashboardAnalyticsCache ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.FleetDashboardAnalyticsCacheReq ->
  Flow APISuccess
postDriverFleetDashboardAnalyticsCache merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  -- validate fleet owner exists
  _ <- QPerson.findById (Id req.fleetOwnerId) >>= fromMaybeM (PersonNotFound req.fleetOwnerId)

  Analytics.updateFleetOwnerAnalyticsKeys req.fleetOwnerId req.activeDriverCount req.activeVehicleCount req.currentOnlineDriver
  pure Success

postDriverDashboardFleetEstimateRoute ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.EstimateRouteReq ->
  Environment.Flow Maps.GetRoutesResp
postDriverDashboardFleetEstimateRoute merchantShortId opCity _fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let getRoutesReq = Maps.GetRoutesReq {waypoints = NE.fromList [req.start, req.end], mode = Just Maps.CAR, calcPoints = True}
  Maps.getRoutes merchant.id merchantOpCityId Nothing getRoutesReq

postDriverFleetTripTransactionsV2 ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Common.TripStatus ->
  Maybe Text ->
  Maybe Text ->
  Int ->
  Int ->
  Flow Common.TripTransactionRespT
postDriverFleetTripTransactionsV2 merchantShortId opCity mbFrom mbTo mbVehicleNumber mbFleetOwnerId mbMemberPersonId mbStatus mbDriverId mbDutyType limit offset = do
  memberPersonId <- mbMemberPersonId & fromMaybeM (InvalidRequest "Member Person ID is required")
  merchant <- findMerchantByShortId merchantShortId
  _ <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  fleetOwnerInfo <- getFleetOwnerIds memberPersonId mbFleetOwnerId
  let fleetNameMap = Map.fromList fleetOwnerInfo
      fleetOwnerIds = map fst fleetOwnerInfo
  tripTransactions <- QTT.findAllTripTransactionByFleetOwnerIdAndTripType fleetOwnerIds DTT.PILOT (Just limit) (Just offset) (Id <$> mbDriverId) mbFrom mbTo (castTripStatus' <$> mbStatus) mbVehicleNumber mbDutyType
  let trips = map (buildTripTransactionDetailsT fleetNameMap) tripTransactions
      summary = Common.Summary {totalCount = 10000000, count = length trips}
  pure $ Common.TripTransactionRespT {trips = trips, totalTrips = length tripTransactions, summary = summary}
  where
    buildTripTransactionDetailsT fleetNameMap tripTransaction =
      Common.TripTransactionDetailT
        { tripTransactionId = cast tripTransaction.id,
          routeCode = tripTransaction.routeCode,
          tripStartTime = tripTransaction.tripStartTime,
          tripEndTime = tripTransaction.tripEndTime,
          tripStatus = castTripStatus tripTransaction.status,
          fleetOwnerId = tripTransaction.fleetOwnerId.getId,
          fleetOwnerName = fromMaybe "" (Map.lookup (tripTransaction.fleetOwnerId.getId) fleetNameMap),
          tripType = castTripType <$> tripTransaction.tripType,
          scheduledTripTime = tripTransaction.scheduledTripTime,
          dutyType = tripTransaction.dutyType,
          vipName = tripTransaction.vipName,
          startAddress = tripTransaction.pilotSource >>= (\point -> Just (Common.Address {point = point, address = tripTransaction.startAddress})),
          endAddress = tripTransaction.pilotDestination >>= (\point -> Just (Common.Address {point = point, address = tripTransaction.endAddress})),
          estimatedRouteDetails = tripTransaction.tripEstimatedRouteDetails >>= (\estimatedRoute -> Just (Common.EstimatedRouteDetails {distance = estimatedRoute.distance, duration = estimatedRoute.duration, polyline = estimatedRoute.polyline}))
        }
    castTripType = \case
      DTT.PILOT -> Common.PILOT
      DTT.WIMB -> Common.WIMB
    castTripStatus = \case
      TRIP_ASSIGNED -> Common.TRIP_ASSIGNED
      IN_PROGRESS -> Common.IN_PROGRESS
      PAUSED -> Common.PAUSED
      COMPLETED -> Common.COMPLETED
      CANCELLED -> Common.CANCELLED
      UPCOMING -> Common.UPCOMING
    castTripStatus' = \case
      Common.TRIP_ASSIGNED -> TRIP_ASSIGNED
      Common.IN_PROGRESS -> IN_PROGRESS
      Common.PAUSED -> PAUSED
      Common.COMPLETED -> COMPLETED
      Common.CANCELLED -> CANCELLED
      Common.UPCOMING -> UPCOMING

postDriverFleetApproveDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.ApproveDriverReq ->
  Flow APISuccess
postDriverFleetApproveDriver merchantShortId opCity fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let driverId = cast req.driverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  fleetDriverAssociation <- QFDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId False >>= fromMaybeM (InvalidRequest "No pending fleet association found")
  when (isNothing fleetDriverAssociation.requestReason) $ throwError $ InvalidRequest "Cannot approve or reject fleet-initiated request. Driver must consent first."

  (pnType, messageBuilder) <- case req.approve of
    Just True -> do
      QFDV.approveFleetDriverAssociation driverId (Id fleetOwnerId) req.reason
      when (transporterConfig.deleteDriverBankAccountWhenLinkToFleet == Just True) $
        QDBA.deleteById driverId

      Analytics.handleDriverAnalyticsAndFlowStatus
        transporterConfig
        driverId
        Nothing
        ( \driverInfo -> do
            Analytics.incrementFleetOwnerAnalyticsActiveDriverCount (Just fleetOwnerId) driverId
            mbOperator <- FOV.findByFleetOwnerId fleetOwnerId True
            when (isNothing mbOperator) $
              logTagError "AnalyticsApproveDriver" "Operator not found for fleet owner"
            whenJust mbOperator $ \operator -> do
              when driverInfo.enabled $
                Analytics.incrementOperatorAnalyticsDriverEnabled transporterConfig operator.operatorId
              Analytics.incrementOperatorAnalyticsActiveDriver transporterConfig operator.operatorId
        )
        ( \driverInfo ->
            DDriverMode.incrementFleetOperatorStatusKeyForDriver DP.FLEET_OWNER fleetOwnerId driverInfo.driverFlowStatus
        )
      pure ("FLEET_REQUEST_APPROVED", \pn -> pn.body)
    _ -> do
      QFDV.rejectFleetDriverAssociation driverId (Id fleetOwnerId) req.reason
      pure
        ( "FLEET_REQUEST_REJECTED",
          \pn -> pn.body <> maybe "" (" " <>) req.reason
        )

  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId pnType Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let notification =
          Notification.NotifReq
            { title = merchantPN.title,
              message = messageBuilder merchantPN,
              entityId = driverId.getId
            }
    Notification.notifyDriverOnEvents merchantOpCityId driverId driver.deviceToken notification merchantPN.fcmNotificationType
  pure Success

postDriverFleetDriverUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.UpdateDriverReq ->
  Flow APISuccess
postDriverFleetDriverUpdate merchantShortId opCity driverId fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  _ <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast driverId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  validateFleetDriverAssociation fleetOwnerId driver.id
  when (isJust req.firstName || isJust req.lastName || isJust req.email) $ do
    let updDriver =
          driver
            { DP.firstName = fromMaybe driver.firstName req.firstName,
              DP.lastName = req.lastName,
              DP.email = req.email
            }
    QPerson.updatePersonDetails updDriver
  for_ req.dob $ \dob ->
    QDriverInfo.updateDob personId (Just (UTCTime dob 0))
  pure Success

getDriverFleetVehicleListStats :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> Day -> Day -> Flow Common.FleetVehicleStatsRes
getDriverFleetVehicleListStats merchantShortId opCity fleetOwnerId mbVehicleNo mbLimit mbOffset fromDay toDay = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (not transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics) $ throwError (InvalidRequest "Analytics is not allowed for this merchant")
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbRcId <- case mbVehicleNo of
    Just vehicleNo -> do
      mbRc <- VRCQuery.findLastVehicleRCFleet' vehicleNo fleetOwnerId
      pure $ (.id.getId) <$> mbRc
    Nothing -> pure Nothing
  agg <- case transporterConfig.analyticsConfig.useDbForEarningAndMetrics of
    True -> QFRDSExtra.sumVehicleStatsByFleetOwnerIdAndDateRange fleetOwnerId mbRcId limit offset fromDay toDay
    _ -> CFRDSExtra.aggerateVehicleStatsByFleetOwnerIdAndDateRange fleetOwnerId mbRcId limit offset fromDay toDay
  let rcIds = map (\vehicleStat -> Id vehicleStat.rcId) agg
  vehicleRegistrationCertificates <- VRCQuery.findAllById rcIds
  let vehicleMap = Map.fromList $ map (\rc -> (rc.id.getId, rc)) vehicleRegistrationCertificates
  let agg' = convertToFleetVehicleStatsItem transporterConfig agg vehicleMap

  pure $ Common.FleetVehicleStatsRes {fleetOwnerId = fleetOwnerId, listItem = agg', summary = Common.Summary {totalCount = 10000, count = length agg}}
  where
    maxLimit = 10
    defaultLimit = 5

convertToFleetVehicleStatsItem :: DTCConfig.TransporterConfig -> [FleetRcDailyStatsAggregated] -> Map.Map Text DVRC.VehicleRegistrationCertificate -> [Common.FleetVehicleStatsItem]
convertToFleetVehicleStatsItem transporterConfig agg vehicleMap =
  map mkItem agg
  where
    calculateEarningsPerKm :: Meters -> HighPrecMoney -> Maybe HighPrecMoney
    calculateEarningsPerKm distance earnings =
      let meters = distance.getMeters
          km = toRational meters / 1000
       in if km == 0
            then Nothing
            else Just (HighPrecMoney (earnings.getHighPrecMoney / km))

    mkItem :: FleetRcDailyStatsAggregated -> Common.FleetVehicleStatsItem
    mkItem stats =
      let rcIdKey = stats.rcId
          mRc = Map.lookup rcIdKey vehicleMap
          mbEarningsPerKm = calculateEarningsPerKm stats.totalDistance stats.totalEarnings
       in Common.FleetVehicleStatsItem
            { Common.vehicleNo = mRc >>= DVRC.unencryptedCertificateNumber,
              Common.vehicleModel = mRc >>= DVRC.vehicleModel,
              Common.vehicleManufacturer = mRc >>= DVRC.vehicleManufacturer,
              rcId = Just rcIdKey,
              totalEarnings = stats.totalEarnings,
              currency = Just transporterConfig.currency,
              completedRides = stats.totalCompletedRides,
              rideDistance = stats.totalDistance,
              rideDuration = stats.totalDuration,
              earningPerKm = mbEarningsPerKm
            }

getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Flow Common.OnboardedDriversAndUnlinkedVehiclesRes
getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles merchantShortId _ fleetOwnerId mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  listOfAllVehicle <- RCQuery.findAllRCByStatusForFleetMF [fleetOwnerId] Nothing (fromIntegral $ fromMaybe 100000 mbLimit) (fromIntegral $ fromMaybe 0 mbOffset) merchant.id Nothing
  activeDriverIds <- QFDAExtra.getActiveDriverIdsByFleetOwnerId fleetOwnerId
  (unLinkedVehicles, linkedDriverIds) <- getDriverVehicleCombination listOfAllVehicle
  let unlinkedDriverIds = filter (\driverId -> driverId `notElem` linkedDriverIds) activeDriverIds
  unlinkedDrivers <- catMaybes <$> mapM QPerson.findById unlinkedDriverIds

  finalUnlinkedDrivers <- forM unlinkedDrivers $ \driver -> do
    mobileNumber <- mapM decrypt driver.mobileNumber
    pure $
      Common.OnboardedDriver
        { driverId = cast @DP.Person @Common.Driver driver.id,
          mobileNumber = mobileNumber
        }
  let finalUnlinkedVehicles =
        map
          ( \vrc ->
              Common.UnlinkedVehicle
                { vehicleNo = Just vrc,
                  rcId = Nothing
                }
          )
          unLinkedVehicles
  pure $
    Common.OnboardedDriversAndUnlinkedVehiclesRes
      { onboardedDrivers = finalUnlinkedDrivers,
        unlinkedVehicles = finalUnlinkedVehicles
      }
  where
    getDriverVehicleCombination :: [DVRC.VehicleRegistrationCertificate] -> Flow ([Text], [Id DP.Person])
    getDriverVehicleCombination vrcList = do
      vehicleDriverCombination <- forM vrcList $ \vrc -> do
        decryptedVehicleRC <- decrypt vrc.certificateNumber
        rcActiveAssociation <- QRCAssociation.findActiveAssociationByRC vrc.id True
        pure (decryptedVehicleRC, rcActiveAssociation >>= (\assoc -> Just assoc.driverId))
      let unLinkedVehicles = map fst $ filter (\(_, driverId) -> isNothing driverId) vehicleDriverCombination
      let linkedDriverIds = snd $ second (catMaybes) (unzip vehicleDriverCombination)
      pure $ (unLinkedVehicles, linkedDriverIds)

getDriverFleetDriverDetails :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow Common.DriverDetailsRes
getDriverFleetDriverDetails merchantShortId opCity fleetOwnerId driverId = do
  validateFleetDriverAssociation fleetOwnerId (cast driverId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == person.merchantId && merchantOpCityId == person.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  mbDriverLicense <- QDriverLicense.findByDriverId personId
  dob <- case mbDriverLicense of
    Just driverLicense -> pure $ utctDay <$> driverLicense.driverDob
    Nothing -> do
      driverInfo <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
      pure $ utctDay <$> driverInfo.driverDob
  pure $
    Common.DriverDetailsRes
      { driverId = cast person.id,
        firstName = person.firstName,
        lastName = person.lastName,
        email = person.email,
        ..
      }

----------------------------------------------------------------------
getDriverFleetScheduledBookingList ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Day ->
  Maybe Day ->
  Maybe DTC.TripCategory ->
  Maybe LatLong ->
  Flow Common.FleetScheduledBookingListRes
getDriverFleetScheduledBookingList merchantShortId opCity _ mbLimit mbOffset mbFromDay mbToDay mbTripCategory mbCurrentLocation = do
  -- check if fleet owner or stop sending fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  if transporterConfig.disableListScheduledBookingAPI
    then pure $ Common.FleetScheduledBookingListRes {bookings = []}
    else do
      case (mbFromDay, mbToDay) of -- added to support more days with same API later
        (Just from, Just to) -> do
          when (from > to) $ throwError (InvalidRequest "From date should be less than to date")

          let limit = fromMaybe 10 mbLimit
              offset = fromMaybe 0 mbOffset
              possibleScheduledTripCategories = [DTC.Rental DTC.OnDemandStaticOffer, DTC.InterCity DTC.OneWayOnDemandStaticOffer Nothing, DTC.OneWay DTC.OneWayOnDemandStaticOffer]
              tripCategory = maybe possibleScheduledTripCategories (: []) mbTripCategory
          cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
          let allVehicleVariants = nub $ concatMap (.allowedVehicleVariant) cityServiceTiers
              safelimit = toInteger transporterConfig.recentScheduledBookingsSafeLimit
          scheduledBookings <- UIDriver.getScheduledBookings from merchantOpCityId allVehicleVariants mbCurrentLocation transporterConfig Nothing tripCategory (toInteger limit) (toInteger offset) safelimit
          bookings <- mapM (UIDriver.buildBookingAPIEntityFromBooking mbCurrentLocation) (catMaybes scheduledBookings)
          fleetBookings <- mapM convertToFleetScheduledBooking (catMaybes bookings)

          pure $ Common.FleetScheduledBookingListRes {bookings = fleetBookings}
        _ -> pure $ Common.FleetScheduledBookingListRes {bookings = []}
  where
    convertToFleetScheduledBooking :: UIDriver.ScheduleBooking -> Flow Common.FleetScheduledBooking
    convertToFleetScheduledBooking UIDriver.ScheduleBooking {..} = do
      fleetBookingDetails <- convertBookingAPIEntity bookingDetails
      fleetFareDetails <- mapM convertRateCardItem fareDetails
      pure $
        Common.FleetScheduledBooking
          { bookingDetails = fleetBookingDetails,
            fareDetails = fleetFareDetails
          }

    convertBookingAPIEntity :: UIDriver.BookingAPIEntity -> Flow Common.FleetBookingAPIEntity
    convertBookingAPIEntity UIDriver.BookingAPIEntity {..} = do
      fromLoc <- convertLocation fromLocation
      toLoc <- mapM convertLocation toLocation
      pure $
        Common.FleetBookingAPIEntity
          { id = id.getId,
            fromLocation = fromLoc,
            toLocation = toLoc,
            estimatedFare = estimatedFare,
            currency = currency,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            startTime = startTime,
            vehicleServiceTier = vehicleServiceTier,
            vehicleServiceTierName = vehicleServiceTierName,
            tripCategory = tripCategory,
            distanceToPickup = distanceToPickup,
            isScheduled = isScheduled
          }

    convertLocation :: DLoc.Location -> Flow Common.LocationAPIEntity
    convertLocation DLoc.Location {..} = do
      let DLoc.LocationAddress {..} = address
      pure $
        Common.LocationAPIEntity
          { id = id.getId,
            lat = lat,
            lon = lon,
            street = street,
            door = door,
            city = city,
            state = state,
            country = country,
            building = building,
            areaCode = areaCode,
            area = area,
            instructions = instructions,
            extras = extras
          }

    convertRateCardItem :: DOVT.RateCardItem -> Flow Common.RateCardItem
    convertRateCardItem DOVT.RateCardItem {..} = do
      pure $
        Common.RateCardItem
          { title = title,
            price = price,
            priceWithCurrency = priceWithCurrency
          }

----------------------------------------------------------------------
postDriverFleetScheduledBookingAssign ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.AssignScheduledBookingReq ->
  Flow APISuccess
postDriverFleetScheduledBookingAssign merchantShortId opCity fleetOwnerId Common.AssignScheduledBookingReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverPersonId = cast @Common.Driver @DP.Person driverId
  fleetDriverAssociation <- QFDAExtra.findByDriverId driverPersonId True >>= fromMaybeM (InvalidRequest "Driver not associated with fleet")
  unless (fleetDriverAssociation.fleetOwnerId == fleetOwnerId) $ throwError (InvalidRequest "Driver does not belong to this fleet owner")
  void $ UIDriver.acceptScheduledBooking (driverPersonId, merchant.id, merchantOpCityId) clientId (Id bookingId)
  pure Success

----------------------------------------------------------------------
postDriverFleetScheduledBookingCancel ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.CancelScheduledBookingReq ->
  Flow APISuccess
postDriverFleetScheduledBookingCancel _merchantShortId _opCity fleetOwnerId Common.CancelScheduledBookingReq {..} = do
  -- Convert cancellation reason code
  let cancelRideReq =
        RideCancel.CancelRideReq
          { reasonCode = coerce @CancellationReasonCode @DCReason.CancellationReasonCode reasonCode,
            additionalInfo,
            doCancellationRateBasedBlocking = Nothing
          }

  -- Call driverCancelRideHandler
  void $ RideCancel.driverCancelRideHandler RideCancel.cancelRideHandle (Id fleetOwnerId) (Id rideId) cancelRideReq
  pure Success
