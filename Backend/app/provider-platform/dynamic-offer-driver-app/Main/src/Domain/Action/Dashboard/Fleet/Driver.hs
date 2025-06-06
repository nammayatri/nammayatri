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
    getDriverFleetDriverVehicleAssociation,
    getDriverFleetDriverAssociation,
    getDriverFleetVehicleAssociation,
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
    getDriverFleetWmbRouteDetails,
    postDriverFleetGetNearbyDrivers,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Endpoints.Driver as Common
import Control.Applicative (optional)
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Driver as Common
import Data.Csv
import Data.List (groupBy, sortOn)
import Data.List.NonEmpty (fromList, toList)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration as DRBReg
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.FleetDriverAssociation as FDA
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Action.UI.WMB as DWMB
import qualified Domain.Types.Alert as DTA
import qualified Domain.Types.AlertRequest as DTR
import qualified Domain.Types.Common as DrInfo
import qualified Domain.Types.DocumentVerificationConfig as DDoc
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverLocation as DDL
import Domain.Types.DriverRCAssociation
import qualified Domain.Types.FleetBadge as DFB
import qualified Domain.Types.FleetBadgeType as DFBT
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.FleetDriverAssociation
import Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as TR
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.TripTransaction
import qualified Domain.Types.TripTransaction as DTT
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleRegistrationCertificate
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
import Kernel.Utils.Validation
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LTST
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.WMB as WMB
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.Ride as CQRide
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner)
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.AlertRequest as QAR
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOperatorAssociation as DOV
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverRCAssociationExtra as DRCAE
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.FleetBadge as QFB
import qualified Storage.Queries.FleetBadgeAssociation as QFBA
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.FleetOperatorAssociation as FOV
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperatorAssociation
import qualified Storage.Queries.FleetOwnerInformation as FOI
import Storage.Queries.FleetRCAssociationExtra as FRAE
import qualified Storage.Queries.FleetRouteAssociation as QFRA
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
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
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity reqDriverPhoneNo requestorId mbFleetOwnerId mbMobileCountryCode req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  phoneNumberHash <- getDbHash reqDriverPhoneNo
  let mobileCountryCode = fromMaybe DCommon.mobileIndianCode mbMobileCountryCode
  entityDetails <- QPerson.findByMobileNumberAndMerchantAndRoles mobileCountryCode phoneNumberHash merchant.id [DP.DRIVER, DP.FLEET_OWNER] >>= fromMaybeM (DriverNotFound reqDriverPhoneNo)
  let merchantId = entityDetails.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == entityDetails.merchantOperatingCityId) $ throwError (PersonDoesNotExist entityDetails.id.getId)
  (getEntityData, getMbFleetOwnerId) <- checkEnitiesAssociationValidation requestorId mbFleetOwnerId entityDetails
  rc <- RCQuery.findLastVehicleRCWrapper req.registrationNo
  case (getEntityData.role, getMbFleetOwnerId) of
    (DP.DRIVER, Nothing) -> do
      -- DCO case
      whenJust rc $ \rcert -> void $ checkRCAssociationForDriver getEntityData.id rcert True
      void $ DCommon.runVerifyRCFlow getEntityData.id merchant merchantOpCityId opCity req True -- Pass fleet.id if addvehicle under fleet or pass driver.id if addvehcile under driver
      logTagInfo "dashboard -> addVehicleUnderDCO : " (show getEntityData.id)
      pure Success
    (_, Just fleetOwnerId) -> do
      -- fleet and fleetDriver case
      whenJust rc $ \rcert -> checkRCAssociationForFleet fleetOwnerId rcert
      Redis.set (DomainRC.makeFleetOwnerKey req.registrationNo) fleetOwnerId -- setting this value here , so while creation of creation of vehicle we can add fleet owner id
      void $ DCommon.runVerifyRCFlow getEntityData.id merchant merchantOpCityId opCity req True -- Pass fleet.id if addvehicle under fleet or pass driver.id if addvehcile under driver
      let logTag = case getEntityData.role of
            DP.FLEET_OWNER -> "dashboard -> addVehicleUnderFleet"
            DP.DRIVER -> "dashboard -> addVehicleUnderFleetDriver"
            _ -> "dashboard -> addVehicleUnderUnknown"
      logTagInfo logTag (show getEntityData.id)
      pure Success
    _ -> throwError (InvalidRequest "Invalid Data")

checkRCAssociationForDriver :: Id DP.Person -> VehicleRegistrationCertificate -> Bool -> Flow Bool
checkRCAssociationForDriver driverId vehicleRC checkFleet = do
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

checkEnitiesAssociationValidation :: Text -> Maybe Text -> DP.Person -> Flow (DP.Person, Maybe Text)
checkEnitiesAssociationValidation requestorId mbFleetOwnerId entityDetails = do
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)

  case requestedPerson.role of
    -- Fleet add vehcile him or under FleetDriver (Driver who has active association with fleet)
    DP.FLEET_OWNER -> do
      fleetOwnerId <- maybe (pure requestorId) (\val -> if requestorId == val then pure val else throwError AccessDenied) mbFleetOwnerId -- Have to discuss
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
          validateFleetDriverAssociation fleetOwnerId entityDetails
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

validateFleetDriverAssociation :: Text -> DP.Person -> Flow ()
validateFleetDriverAssociation fleetOwnerId driverPerson = do
  isAssociated <- FDV.findByDriverIdAndFleetOwnerId driverPerson.id fleetOwnerId True
  when (isNothing isAssociated) $
    throwError (DriverNotActiveInFleet driverPerson.id.getId fleetOwnerId)

checkRCAssociationForFleet :: Text -> VehicleRegistrationCertificate -> Flow ()
checkRCAssociationForFleet fleetOwnerId vehicleRC = do
  when (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId /= Just fleetOwnerId) $ throwError VehicleBelongsToAnotherFleet
  activeAssociationsOfRC <- DRCAE.findAllActiveAssociationByRCId vehicleRC.id
  let rcAssociatedDriverIds = map (.driverId) activeAssociationsOfRC
  forM_ rcAssociatedDriverIds $ \driverId -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
    when (isNothing isFleetDriver) $ throwError (VehicleLinkedToInvalidDriver)

---------------------------------------------------------------------
getDriverFleetGetDriverRequests ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe DTA.AlertRequestType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.DriverRequestResp
getDriverFleetGetDriverRequests merchantShortId opCity fleetOwnerId mbFrom mbTo mbAlertRequestType mbRouteCode mbDriverId mbBadgeName mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  tripAlertRequests <-
    case mbBadgeName of
      Just badgeName -> do
        mbBadge <- QFB.findOneBadgeByNameAndBadgeTypeAndFleetOwnerId (Id fleetOwnerId) badgeName DFBT.DRIVER
        case mbBadge of
          Just badge -> B.runInReplica $ QTAR.findTripAlertRequestsByFleetOwnerId merchantOpCityId fleetOwnerId mbFrom mbTo mbAlertRequestType mbDriverId (Just badge.id) mbRouteCode mbLimit mbOffset
          Nothing -> pure []
      Nothing -> B.runInReplica $ QTAR.findTripAlertRequestsByFleetOwnerId merchantOpCityId fleetOwnerId mbFrom mbTo mbAlertRequestType mbDriverId Nothing mbRouteCode mbLimit mbOffset
  driverRequestList <-
    mapM
      ( \tripAlertRequest -> do
          alertRequest <- QAR.findByPrimaryKey tripAlertRequest.alertRequestId >>= fromMaybeM (DriverRequestNotFound tripAlertRequest.alertRequestId.getId)
          buildDriverRequestListItem tripAlertRequest.tripTransactionId tripAlertRequest.driverId tripAlertRequest.routeCode tripAlertRequest.isViolated alertRequest
      )
      tripAlertRequests
  pure $ Common.DriverRequestResp driverRequestList
  where
    buildDriverRequestListItem tripTransactionId driverId routeCode isViolated DTR.AlertRequest {..} = do
      pure $
        Common.DriverRequestDetails
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
      DTA.AWAITING_APPROVAL -> QAR.updateStatusWithReason req.status (Just req.reason) (Id req.approvalRequestId)
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
  Redis.set (DomainRC.makeFleetOwnerKey req.vehicleRegistrationCertNumber) fleetOwnerId
  let rcReq =
        DomainRC.DriverRCReq
          { vehicleRegistrationCertNumber = req.vehicleRegistrationCertNumber,
            imageId = cast req.imageId,
            operatingCity = req.operatingCity,
            dateOfRegistration = req.dateOfRegistration,
            airConditioned = req.airConditioned,
            oxygen = req.oxygen,
            ventilator = req.ventilator,
            multipleRC = req.multipleRC,
            vehicleCategory = req.vehicleCategory,
            vehicleDetails = Nothing
          }
  void $ DomainRC.verifyRC False (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq
  logTagInfo "dashboard -> Register RC For Fleet : " (show driver.id)
  pure Success

---------------------------------------------------------------------
getDriverFleetGetAllVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Bool ->
  Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle merchantShortId _ fleetOwnerId mbLimit mbOffset mbRegNumberString mbIsActive = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  mbRegNumberStringHash <- mapM getDbHash mbRegNumberString
  case mbIsActive of
    Just True -> do
      activeVehicleList <- QRCAssociation.findAllActiveAssociationByFleetOwnerId fleetOwnerId (Just limit) (Just offset) mbRegNumberString mbRegNumberStringHash
      vehicles <- traverse convertToVehicleAPIEntityFromAssociation activeVehicleList
      return $ Common.ListVehicleRes vehicles
    Just False -> do
      inactiveVehicleList <- QRCAssociation.findAllInactiveAssociationByFleetOwnerId fleetOwnerId limit offset mbRegNumberString mbRegNumberStringHash
      vehicles <- traverse convertToVehicleAPIEntityFromAssociation inactiveVehicleList
      return $ Common.ListVehicleRes vehicles
    Nothing -> do
      vehicleList <- RCQuery.findAllValidRcByFleetOwnerIdAndSearchString (toInteger limit) (toInteger offset) merchant.id fleetOwnerId mbRegNumberString mbRegNumberStringHash
      vehicles <- traverse convertToVehicleAPIEntity vehicleList
      return $ Common.ListVehicleRes vehicles

convertToVehicleAPIEntity :: VehicleRegistrationCertificate -> Flow Common.VehicleAPIEntity
convertToVehicleAPIEntity VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  mActiveAssociation <- QRCAssociation.findActiveAssociationByRC id True
  let isActive = isJust mActiveAssociation
  pure
    Common.VehicleAPIEntity
      { variant = DCommon.castVehicleVariantDashboard vehicleVariant,
        model = vehicleModel,
        color = vehicleColor,
        registrationNo = certificateNumber',
        isActive = (Just isActive)
      }

convertToVehicleAPIEntityFromAssociation :: (DriverRCAssociation, VehicleRegistrationCertificate) -> Flow Common.VehicleAPIEntity
convertToVehicleAPIEntityFromAssociation (association, rc) = do
  certificateNumber' <- decrypt rc.certificateNumber
  pure
    Common.VehicleAPIEntity
      { variant = DCommon.castVehicleVariantDashboard rc.vehicleVariant,
        model = rc.vehicleModel,
        color = rc.vehicleColor,
        registrationNo = certificateNumber',
        isActive = (Just association.isRcActive)
      }

---------------------------------------------------------------------
getDriverFleetGetAllDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbMobileNumber mbName mbSearchString mbIsActive = do
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  mobileNumberHash <- case mbSearchString of
    Just _ -> pure Nothing
    Nothing -> case mbMobileNumber of
      Just phNo -> Just <$> getDbHash phNo
      Nothing -> pure Nothing
  case mbIsActive of
    Just True -> do
      pairs <- FDV.findAllActiveDriverByFleetOwnerId fleetOwnerId (Just limit) (Just offset) mobileNumberHash mbName mbSearchString (Just True)
      fleetDriversInfos <- mapM convertToDriverAPIEntity pairs
      return $ Common.FleetListDriverRes fleetDriversInfos
    Just False -> do
      pairs <- FDV.findAllInactiveDriverByFleetOwnerId fleetOwnerId (Just limit) (Just offset) mobileNumberHash mbName mbSearchString
      fleetDriversInfos <- mapM convertToDriverAPIEntity pairs
      return $ Common.FleetListDriverRes fleetDriversInfos
    Nothing -> do
      pairs <- FDV.findAllDriverByFleetOwnerId fleetOwnerId (Just limit) (Just offset) mobileNumberHash mbName mbSearchString
      fleetDriversInfos <- mapM convertToDriverAPIEntity pairs
      return $ Common.FleetListDriverRes fleetDriversInfos

convertToDriverAPIEntity :: (FleetDriverAssociation, DP.Person) -> Flow Common.FleetDriversAPIEntity
convertToDriverAPIEntity (_, person) = do
  unencryptedMobileNumber <- mapM decrypt person.mobileNumber
  vehicle <- QVehicle.findById person.id
  let isActive = isJust vehicle
  pure $
    Common.FleetDriversAPIEntity
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = unencryptedMobileNumber,
        mobileCountryCode = person.mobileCountryCode,
        isActive = Just isActive
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
      isDriverOperator <- DOV.checkDriverOperatorAssociation personId (Id entityId)
      when (not isDriverOperator) $ throwError DriverNotPartOfOperator
      unlinkVehicleFromDriver merchant personId vehicleNo opCity DP.OPERATOR
    _ -> throwError $ InvalidRequest "Invalid Data"
  pure Success

unlinkVehicleFromDriver :: DM.Merchant -> Id DP.Person -> Text -> Context.City -> DP.Role -> Flow ()
unlinkVehicleFromDriver merchant personId vehicleNo opCity role = do
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)
  rc <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (RCNotFound vehicleNo)
  driverInfo <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (transporterConfig.deactivateRCOnUnlink == Just True) $ DomainRC.deactivateCurrentRC personId
  QVehicle.deleteById personId
  when (driverInfo.onboardingVehicleCategory /= Just DVC.BUS) $ QDriverInfo.updateEnabledVerifiedState personId False (Just False) -- TODO :: Is it required for Normal Fleet ?
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
    updatedVehicleRegistrationCertificate VehicleRegistrationCertificate {..} = VehicleRegistrationCertificate {fleetOwnerId = Nothing, ..}

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
                                  try @_ @SomeException
                                    (buildVehicleRouteMapping (Id fleetOwnerId) merchant.id merchantOpCity.id registerRcReq.vehicleRegistrationCertNumber route)
                                    >>= \case
                                      Left err -> return $ Just ("Failed to Add Vehicle Route Mapping for Vehicle: " <> registerRcReq.vehicleRegistrationCertNumber <> ", Route: " <> route.code <> ", Error: " <> (T.pack $ displayException err))
                                      Right _ -> pure Nothing
                            Nothing -> do
                              try @_ @SomeException
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
                    (Nothing, Nothing) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq mobileNumber Nothing -- Add vehicles under requested Fleet
                    (Nothing, Just driverNo) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq driverNo (Just mobileNumber) -- Map driver <-> vehicle under requested fleet
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
                (Just fleetNo, Nothing) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq fleetNo (Just fleetNo) -- Add vehicles under Fleet
                (Nothing, Just driverNo) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq driverNo Nothing -- Add vehicles under DCO
                (Just fleetNo, Just driverNo) -> handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq driverNo (Just fleetNo) -- Map driver <-> vehicle under fleer
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
      DTC.TransporterConfig ->
      Maybe Text ->
      Text ->
      Common.RegisterRCReq ->
      Text ->
      Maybe Text ->
      Flow (Either Text ())
    handleAddVehicleWithTry merchant transporterConfig mbCountryCode requestorId registerRcReq phoneNo mbFleetNo = do
      result <- try @_ @SomeException $ do
        when transporterConfig.enableExistingVehicleInBulkUpload $
          isVehicleAlreadyAssociatedWithFleetOrDriver registerRcReq.vehicleRegistrationCertNumber

        mbFleetOwnerId <- case mbFleetNo of
          Just fleetNo -> do
            phoneHash <- getDbHash fleetNo
            let mobileCountryCode = fromMaybe DCommon.mobileIndianCode mbCountryCode
            QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneHash merchant.id DP.FLEET_OWNER >>= fromMaybeM (FleetOwnerNotFound fleetNo) <&> (Just . (.id.getId))
          Nothing -> pure Nothing

        let addVehicleReq = convertToAddVehicleReq registerRcReq

        postDriverFleetAddVehicle merchant.shortId opCity phoneNo requestorId mbFleetOwnerId mbCountryCode addVehicleReq

      case result of
        Left e -> return $ Left $ "Error: " <> T.pack (displayException e)
        Right _ -> return $ Right ()

    isVehicleAlreadyAssociatedWithFleetOrDriver :: Text -> Flow () -- checking vehicle present in the system or not
    isVehicleAlreadyAssociatedWithFleetOrDriver vehicleNo = do
      rc <- RCQuery.findLastVehicleRCWrapper vehicleNo
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
              multipleRC = Nothing,
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
postDriverFleetRemoveDriver merchantShortId _ requestorId driverId mbFleetOwnerId = do
  requestedPerson <- QPerson.findById (Id requestorId) >>= fromMaybeM (PersonDoesNotExist requestorId)
  (entityRole, entityId) <- validateRequestorRoleAndGetEntityId requestedPerson mbFleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  let personId = cast @Common.Driver @DP.Person driverId
  case entityRole of
    DP.FLEET_OWNER -> do
      DCommon.checkFleetOwnerVerification entityId merchant.fleetOwnerEnabledCheck
      associationList <- QRCAssociation.findAllLinkedByDriverId personId
      forM_ associationList $ \assoc -> do
        rc <- RCQuery.findByRCIdAndFleetOwnerId assoc.rcId $ Just entityId
        when (isJust rc) $ throwError (InvalidRequest "Driver is linked to fleet Vehicle, first unlink then try")
      FDV.endFleetDriverAssociation entityId personId
    DP.OPERATOR -> do
      associationList <- QRCAssociation.findAllLinkedByDriverId personId
      unless (null associationList) $ throwError (InvalidRequest "Driver is linked to Vehicle, first unlink then try")
      DOV.endOperatorDriverAssociation entityId personId
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

getListOfVehicles :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Common.FleetVehicleStatus -> Id DM.Merchant -> Maybe Text -> Maybe Text -> Flow [VehicleRegistrationCertificate]
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
      let countryCode = fromMaybe "+91" mbMobileCountryCode
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
    createDriverVehicleAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => [FleetDriverAssociation] -> [VehicleRegistrationCertificate] -> m [Common.DriveVehicleAssociationListItem]
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
getDriverFleetDriverAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Common.DriverMode ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverAssociation merchantShortId _opCity fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbDriverPhNo mbStats mbFrom mbTo mbMode mbName mbSearchString mbRequestorId = do
  void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  listOfAllDrivers <- getListOfDrivers mbCountryCode mbDriverPhNo fleetOwnerId merchant.id mbIsActive mbLimit mbOffset mbMode mbName mbSearchString
  listItems <- createFleetDriverAssociationListItem listOfAllDrivers
  let summary = Common.Summary {totalCount = 10000, count = length listItems}
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = listItems, summary = summary}
  where
    createFleetDriverAssociationListItem :: ([FleetDriverAssociation], [DP.Person], [DI.DriverInformation]) -> Flow [Common.DriveVehicleAssociationListItem]
    createFleetDriverAssociationListItem (fdaList, personList, driverInfoList) = do
      let driverListWithInfo = zip personList driverInfoList
      now <- getCurrentTime
      let defaultFrom = UTCTime (utctDay now) 0
          from = fromMaybe defaultFrom mbFrom
          to = fromMaybe now mbTo
      forM (zip driverListWithInfo fdaList) $ \((driver, driverInfo'), fda) -> do
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
        let isDriverActive = fda.isActive
        let driverId = Just $ driver.id.getId
        let ls =
              Common.DriveVehicleAssociationListItem
                { vehicleNo = vehicleNo,
                  status = driverStatus,
                  isDriverActive = isDriverActive,
                  isDriverOnRide = Just isDriverOnRide,
                  isDriverOnPickup = Just isDriverOnPickup,
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
                          vehicleInsurance = Nothing
                        },
                  ..
                }
        pure ls
    getVehicleDetails :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => VehicleRegistrationCertificate -> m (Maybe Text, Maybe Common.VehicleVariant)
    getVehicleDetails vrc = do
      decryptedVehicleRC <- decrypt vrc.certificateNumber
      let vehicleType = DCommon.castVehicleVariantDashboard vrc.vehicleVariant
      pure (Just decryptedVehicleRC, vehicleType)

---------------------------------------------------------------------
getDriverFleetVehicleAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Common.FleetVehicleStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetVehicleAssociation merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbVehicleNumber mbIncludeStats mbFrom mbTo mbStatus mbSearchString mbStatusAwareVehicleNo mbRequestorId = do
  void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  listOfAllVehicle <- getListOfVehicles mbVehicleNumber fleetOwnerId mbLimit mbOffset mbStatus merchant.id mbSearchString mbStatusAwareVehicleNo
  listItems <- createFleetVehicleAssociationListItem listOfAllVehicle
  let summary = Common.Summary {totalCount = 10000, count = length listItems}
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = listItems, summary = summary}
  where
    createFleetVehicleAssociationListItem :: [VehicleRegistrationCertificate] -> Flow [Common.DriveVehicleAssociationListItem]
    createFleetVehicleAssociationListItem vrcList = do
      now <- getCurrentTime
      forM vrcList $ \vrc -> do
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
        (driverName, conductorName, driverId, driverPhoneNo, driverStatus, isDriverOnPickup, isDriverOnRide, routeCode) <- case rcActiveAssociation of
          Just activeAssociation -> getFleetDriverInfo fleetOwnerId activeAssociation.driverId False ------- when vehicle is in active state
          Nothing -> do
            latestAssociation <- QRCAssociation.findLatestLinkedByRCId vrc.id now ------- when there is not any active association then i will find out the latest association  (vehicle is in inActive state)
            case latestAssociation of
              Just latestAssoc -> getFleetDriverInfo fleetOwnerId latestAssoc.driverId False
              Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing, Just False, Just False, Nothing) -------- when vehicle is unAssigned
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
                  driverLicense = Nothing,
                  panCard = Nothing,
                  aadhaarCard = Nothing
                }
        let ls =
              Common.DriveVehicleAssociationListItem
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
                  ..
                }
        pure ls

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
postDriverFleetVehicleDriverRcStatus merchantShortId opCity reqDriverId fleetOwnerId mbRequestorId req = do
  void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId personId fleetOwnerId True
  when (isNothing isFleetDriver) $ throwError DriverNotPartOfFleet
  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  vehicle <- RCQuery.findLastVehicleRCWrapper req.rcNo >>= fromMaybeM (VehicleDoesNotExist req.rcNo)
  unless (isJust vehicle.fleetOwnerId && vehicle.fleetOwnerId == Just fleetOwnerId) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId)
  Redis.set (DomainRC.makeFleetOwnerKey req.rcNo) fleetOwnerId
  _ <- DomainRC.linkRCStatus (personId, merchant.id, merchantOpCityId) (DomainRC.RCStatusReq {isActivate = req.isActivate, rcNo = req.rcNo})
  logTagInfo "dashboard -> addVehicle : " (show driver.id)
  pure Success

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
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  whenJust req.mobileNo $ \reqMobileNo -> do
    mobileNumberHash <- getDbHash reqMobileNo
    person <- QPerson.findByMobileNumberAndMerchantAndRole (fromMaybe "+91" req.mobileCountryCode) mobileNumberHash merchant.id DP.FLEET_OWNER
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
  QPerson.updateFleetOwnerDetails personId updDriver
  pure Success

---------------------------------------------------------------------
getDriverFleetOwnerInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo _ _ driverId = do
  let personId = cast @Common.Driver @DP.Person driverId
  mbFleetOwnerInfo <- B.runInReplica $ FOI.findByPrimaryKey personId
  case mbFleetOwnerInfo of
    Nothing -> do
      person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Person is not a fleet owner or operator")
      referral <- QDR.findById personId
      pure
        Common.FleetOwnerInfoRes
          { fleetType = "",
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
            businessLicenseNumber = Nothing
          }
    Just fleetOwnerInfo -> do
      fleetConfig <- QFC.findByPrimaryKey personId
      mbFleetOperatorAssoc <- FOV.findByFleetOwnerId personId.getId True
      (operatorName, operatorContact) <- case mbFleetOperatorAssoc of
        Nothing -> pure (Nothing, Nothing)
        Just fleetOperatorAssoc -> do
          person <- QPerson.findById (Id fleetOperatorAssoc.operatorId) >>= fromMaybeM (PersonDoesNotExist fleetOperatorAssoc.operatorId)
          contact <- mapM decrypt person.mobileNumber
          pure $ (Just (person.firstName <> fromMaybe "" person.middleName <> fromMaybe "" person.lastName), contact)
      makeFleetOwnerInfoRes fleetConfig fleetOwnerInfo operatorName operatorContact
  where
    makeFleetOwnerInfoRes :: Maybe DFC.FleetConfig -> DFOI.FleetOwnerInformation -> Maybe Text -> Maybe Text -> Flow Common.FleetOwnerInfoRes
    makeFleetOwnerInfoRes mbFleetConfig DFOI.FleetOwnerInformation {..} operatorName operatorContact = do
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
      return $
        Common.FleetOwnerInfoRes
          { fleetType = show fleetType,
            referralCode = (.referralCode.getId) <$> referral,
            gstNumber = gstNumber',
            panNumber = panNumber',
            aadhaarNumber = aadhaarNumber',
            businessLicenseNumber = businessLicenseNumber',
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
  merchant <- findMerchantByShortId merchantShortId
  whenJust mbFleetOwnerId $ \fleetOwnerId -> do
    void $ checkRequestorAccessToFleet mbRequestorId fleetOwnerId
    DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
  smsCfg <- asks (.smsCfg)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mobileNumberHash <- getDbHash req.mobileNumber
  mbPerson <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER
  case mbPerson of
    Nothing -> DRBReg.auth merchantShortId opCity req -------------- to onboard a driver that is not the part of the fleet
    Just person -> do
      withLogTag ("personId_" <> getId person.id) $ do
        SA.checkForDriverAssociationOverwrite merchant person.id
        let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
            phoneNumber = req.mobileCountryCode <> req.mobileNumber
        otpCode <- maybe generateOTPCode return useFakeOtpM
        whenNothing_ useFakeOtpM $ do
          (mbSender, message) <-
            MessageBuilder.buildFleetJoiningMessage merchantOpCityId $
              MessageBuilder.BuildFleetJoiningMessageReq
                { otp = otpCode,
                  fleetOwnerName = fleetOwnerName
                }
          let sender = fromMaybe smsCfg.sender mbSender
          Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
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
  case mbAuthId of
    Just authId -> do
      smsCfg <- asks (.smsCfg)
      deviceToken <- fromMaybeM (DeviceTokenNotFound) $ req.deviceToken

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId person

      void $ DRBReg.verify authId True fleetOwnerId (mbOperator <&> (.id)) Common.AuthVerifyReq {otp = req.otp, deviceToken = deviceToken}

      whenJust mbOperator $ \referredOperator -> do
        DOR.makeDriverReferredByOperator merchantOpCityId person.id referredOperator.id

      let phoneNumber = req.mobileCountryCode <> req.mobileNumber
      withLogTag ("personId_" <> getId person.id) $ do
        (mbSender, message) <-
          MessageBuilder.buildFleetJoinAndDownloadAppMessage merchantOpCityId $
            MessageBuilder.BuildDownloadAppMessageReq
              { fleetOwnerName = fleetOwner.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
    Nothing -> do
      let key = makeFleetDriverOtpKey (req.mobileCountryCode <> req.mobileNumber)
      otp <- Redis.get key >>= fromMaybeM OtpNotFound
      when (otp /= req.otp) $ throwError InvalidOtp
      checkAssoc <- B.runInReplica $ QFDV.findByDriverIdAndFleetOwnerId person.id fleetOwnerId True
      when (isJust checkAssoc) $ throwError (InvalidRequest "Driver already associated with fleet")

      SA.endDriverAssociationsIfAllowed merchant merchantOpCityId person

      -- onboarded operator required only for new drivers
      assoc <- FDA.makeFleetDriverAssociation person.id fleetOwnerId Nothing (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QFDV.create assoc
  pure Success

makeFleetDriverOtpKey :: Text -> Text
makeFleetDriverOtpKey phoneNo = "Fleet:Driver:PhoneNo" <> phoneNo

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
  phoneNumberHash <- getDbHash req.driverMobileNumber
  let mobileCountryCode = fromMaybe DCommon.mobileIndianCode req.driverMobileCountryCode
  driver <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneNumberHash merchant.id DP.DRIVER >>= fromMaybeM (DriverNotFound req.driverMobileNumber)
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist driver.id.getId)
  rc <- RCQuery.findLastVehicleRCWrapper req.vehicleRegistrationNumber >>= fromMaybeM (RCNotFound req.vehicleRegistrationNumber)
  when (isNothing rc.fleetOwnerId || (isJust rc.fleetOwnerId && rc.fleetOwnerId /= Just fleetOwnerId)) $ throwError VehicleNotPartOfFleet
  unless (rc.verificationStatus == Documents.VALID) $ throwError (RcNotValid)
  validateFleetDriverAssociation fleetOwnerId driver
  isValidAssociation <- checkRCAssociationForDriver driver.id rc False
  when (not isValidAssociation) $ do
    driverRCAssoc <- makeRCAssociation driver.merchantId driver.merchantOperatingCityId driver.id rc.id (convertTextToUTC (Just "2099-12-12"))
    QRCAssociation.create driverRCAssoc
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
    try @_ @SomeException (LF.driversLocation [tripTransaction.driverId])
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

getDriverFleetGetAllBadge :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe DFBT.FleetBadgeType -> Maybe Bool -> Flow Common.FleetBadgeRes
getDriverFleetGetAllBadge merchantShortId opCity fleetOwnerId limit offset mbSearchString mbBadgeType mbIsActive = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  case mbIsActive of
    Just True -> do
      activeBadges <- QFBA.findAllActiveFleetBadgeAssociation limit offset mbSearchString mbBadgeType
      fleetBadgeInfos <-
        mapM
          ( \(_, badge) -> do
              pure $ Common.FleetBadgesAPIEntity badge.badgeName True badge.badgeType
          )
          activeBadges
      return $ Common.FleetBadgeRes {..}
    Just False -> do
      inactiveBadges <- QFBA.findAllInactiveFleetBadgeAssociation limit offset mbSearchString mbBadgeType
      fleetBadgeInfos <-
        mapM
          ( \(_, badge) -> do
              pure $ Common.FleetBadgesAPIEntity badge.badgeName False badge.badgeType
          )
          inactiveBadges
      return $ Common.FleetBadgeRes {..}
    Nothing -> do
      fleetBadgesByOwner <- QFB.findAllMatchingBadges mbSearchString (toInteger <$> limit) (toInteger <$> offset) merchantOpCity.id fleetOwnerId mbBadgeType
      fleetBadgeInfos <-
        mapM
          ( \badge -> do
              driverBadgeAssociation <- QFBA.findActiveFleetBadgeAssociationById badge.id badge.badgeType
              pure $ Common.FleetBadgesAPIEntity badge.badgeName (isJust driverBadgeAssociation) badge.badgeType
          )
          fleetBadgesByOwner
      return $ Common.FleetBadgeRes {..}

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
            try @_ @SomeException $
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
  try @_ @SomeException
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
        createTripTransactions merchant.id merchantOpCity.id fleetOwnerId req.driverId vehicleRC mbDriverBadge mbConductorBadge req.trips
        pure Success

createTripTransactions :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Id Common.Driver -> VehicleRegistrationCertificate -> Maybe DFB.FleetBadge -> Maybe DFB.FleetBadge -> [Common.TripDetails] -> Flow ()
createTripTransactions merchantId merchantOpCityId fleetOwnerId driverId vehicleRC mbDriverBadge mbConductorBadge trips = do
  initialActiveTrip <- WMB.findNextActiveTripTransaction fleetOwnerId (cast driverId)
  let isActive = case initialActiveTrip of
        Just _ -> True
        Nothing -> False
  (allTransactions, _) <-
    foldM
      ( \(accTransactions, updatedIsActive) trip -> do
          ( if updatedIsActive
              then do
                tripTransactions <- makeTripTransactions trip DTT.UPCOMING
                return (accTransactions <> tripTransactions, updatedIsActive)
              else do
                tripTransactions <- makeTripTransactions trip DTT.TRIP_ASSIGNED
                return (accTransactions <> tripTransactions, True)
            )
      )
      ([], isActive)
      trips
  WMB.findNextActiveTripTransaction fleetOwnerId (cast driverId)
    >>= \case
      Just _ -> QTT.createMany allTransactions
      Nothing -> do
        QTT.createMany allTransactions
        whenJust (listToMaybe allTransactions) $ \tripTransaction -> do
          route <- QRoute.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
          (routeSourceStopInfo, routeDestinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route route.code
          WMB.postAssignTripTransaction tripTransaction route True routeSourceStopInfo.point routeSourceStopInfo.point routeDestinationStopInfo.point True
  where
    makeTripTransactions :: Common.TripDetails -> DTT.TripStatus -> Flow [DTT.TripTransaction]
    makeTripTransactions trip tripStatus = do
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
                tripTransaction <- mkTransaction routeCode (Just roundRouteCode_) endStopCode tripStatus
                pure $ accTransactions <> [tripTransaction]
            )
            []
            [0 .. (2 * roundTrip.frequency) -1]
        (_, _) -> do
          tripTransaction <- mkTransaction route.code route.roundRouteCode routeDestinationStopInfo.code tripStatus
          pure [tripTransaction]

    mkTransaction :: Text -> Maybe Text -> Text -> DTT.TripStatus -> Flow DTT.TripTransaction
    mkTransaction routeCode roundRouteCode endStopCode tripStatus = do
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
            ..
          }

---------------------------------------------------------------------
getDriverFleetTripTransactions ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Int ->
  Int ->
  Flow Common.TripTransactionResp
getDriverFleetTripTransactions merchantShortId opCity fleetOwnerId driverId mbFrom mbTo mbVehicleNumber limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  _ <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  tripTransactions <- QTT.findAllTripTransactionByDriverIdWithinCreationRange (Id fleetOwnerId) (Just limit) (Just offset) (cast driverId) mbFrom mbTo mbVehicleNumber
  let trips = map buildTripTransactionDetails tripTransactions
      summary = Common.Summary {totalCount = 10000, count = length trips}
  pure Common.TripTransactionResp {trips = trips, totalTrips = length tripTransactions, summary = summary}
  where
    buildTripTransactionDetails tripTransaction =
      Common.TripTransactionDetail
        { tripTransactionId = cast tripTransaction.id,
          routeCode = tripTransaction.routeCode,
          tripStartTime = tripTransaction.tripStartTime,
          tripEndTime = tripTransaction.tripEndTime,
          tripStatus = castTripStatus tripTransaction.status
        }

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

  driverTripPlanner <-
    mapM
      ( \case
          [] -> throwError (InternalError "Something Went Wrong while grouping Drivers")
          (driverGroup : driverGroups) -> do
            mobileNumberHash <- getDbHash driverGroup.driverPhoneNo
            person <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash merchant.id DP.DRIVER
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
          try @_ @SomeException
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
                try @_ @SomeException
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
          roundTrip = fmap (\freq -> Common.RoundTripDetail {frequency = freq}) driverGroup.roundTripFreq
        }

---------------------------------------------------------------------
data CreateDriversCSVRow = CreateDriversCSVRow
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe Text,
    fleetPhoneNo :: Maybe Text
  }

data DriverDetails = DriverDetails
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory,
    fleetPhoneNo :: Maybe Text
  }

instance FromNamedRecord CreateDriversCSVRow where
  parseNamedRecord r =
    CreateDriversCSVRow
      <$> r .: "driver_name"
      <*> r .: "driver_phone_number"
      <*> optional (r .: "driver_onboarding_vehicle_category")
      <*> optional (r .: "fleet_phone_no")

postDriverFleetAddDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Common.CreateDriversReq ->
  Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDrivers merchantShortId opCity mbRequestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  driverDetails <- Csv.readCsv @CreateDriversCSVRow @DriverDetails req.file parseDriverInfo
  when (length driverDetails > 100) $ throwError $ MaxDriversLimitExceeded 100 -- TODO: Configure the limit
  let process func =
        foldlM
          ( \unprocessedEntities driverDetail -> do
              try @_ @SomeException
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
      DCommon.checkFleetOwnerVerification fleetOwnerId merchant.fleetOwnerEnabledCheck
      process (processDriverByFleetOwner merchant merchantOpCity fleetOwner)
    Just requestorId -> do
      requestor <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
      case requestor.role of
        DP.FLEET_OWNER -> do
          -- fleetOwner is in req, should be the same as requestor
          whenJust req.fleetOwnerId \id -> do
            unless (id == requestorId) $ throwError AccessDenied
            DCommon.checkFleetOwnerVerification id merchant.fleetOwnerEnabledCheck
          process (processDriverByFleetOwner merchant merchantOpCity requestor)
        DP.OPERATOR ->
          -- fleetOwner is in csv row
          process (processDriverByOperator merchant merchantOpCity requestor)
        _ -> throwError AccessDenied

  pure $ Common.APISuccessWithUnprocessedEntities unprocessedEntities
  where
    processDriverByFleetOwner :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> DriverDetails -> Flow () -- TODO: create single query to update all later
    processDriverByFleetOwner merchant moc fleetOwner req_ = do
      validateDriverName req_.driverName
      whenJust req_.fleetPhoneNo \fleetPhoneNo -> do
        mobileNumberHash <- getDbHash fleetPhoneNo
        fleetOwnerCsv <-
          B.runInReplica $
            QP.findByMobileNumberAndMerchantAndRole DCommon.mobileIndianCode mobileNumberHash merchant.id DP.FLEET_OWNER
              >>= fromMaybeM (FleetOwnerNotFound fleetPhoneNo)
        unless (fleetOwnerCsv.id == fleetOwner.id) $
          throwError AccessDenied
      linkDriverToFleetOwner merchant moc fleetOwner Nothing req_

    processDriverByOperator :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> DriverDetails -> Flow () -- TODO: create single query to update all later
    processDriverByOperator merchant moc operator req_ = do
      validateDriverName req_.driverName
      case req_.fleetPhoneNo of
        Nothing -> do
          linkDriverToOperator merchant moc operator req_
        Just fleetPhoneNo -> do
          mobileNumberHash <- getDbHash fleetPhoneNo
          fleetOwner <-
            B.runInReplica $
              QP.findByMobileNumberAndMerchantAndRole DCommon.mobileIndianCode mobileNumberHash merchant.id DP.FLEET_OWNER
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

    validateDriverName :: Text -> Flow ()
    validateDriverName driverName = do
      result <- try (void $ runRequestValidation Common.validateUpdateDriverNameReq (Common.UpdateDriverNameReq {firstName = driverName, middleName = Nothing, lastName = Nothing}))
      case result of
        Left (_ :: SomeException) -> throwError $ InvalidRequest "Driver name should not contain numbers and should have at least 3 letters"
        Right _ -> pure ()

    linkDriverToFleetOwner :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> Maybe (Id DP.Person) -> DriverDetails -> Flow () -- TODO: create single query to update all later
    linkDriverToFleetOwner merchant moc fleetOwner mbOperatorId req_ = do
      (person, isNew) <- fetchOrCreatePerson moc req_
      (if isNew then pure False else WMB.checkFleetDriverAssociation person.id fleetOwner.id)
        >>= \isAssociated -> unless isAssociated $ do
          unless isNew $ SA.checkForDriverAssociationOverwrite merchant person.id
          fork "Sending Fleet Consent SMS to Driver" $ do
            let driverMobile = req_.driverPhoneNumber
            let onboardedOperatorId = if isNew then mbOperatorId else Nothing
            FDV.createFleetDriverAssociationIfNotExists person.id fleetOwner.id onboardedOperatorId (fromMaybe DVC.CAR req_.driverOnboardingVehicleCategory) False
            sendDeepLinkForAuth person driverMobile moc.merchantId moc.id fleetOwner

    linkDriverToOperator :: DM.Merchant -> DMOC.MerchantOperatingCity -> DP.Person -> DriverDetails -> Flow () -- TODO: create single query to update all later
    linkDriverToOperator merchant moc operator req_ = do
      (person, isNew) <- fetchOrCreatePerson moc req_
      (if isNew then pure False else QDOA.checkDriverOperatorAssociation person.id operator.id)
        >>= \isAssociated -> unless isAssociated $ do
          unless isNew $ SA.checkForDriverAssociationOverwrite merchant person.id
          fork "Sending Operator Consent SMS to Driver" $ do
            let driverMobile = req_.driverPhoneNumber
            QDOA.createDriverOperatorAssociationIfNotExists moc person.id operator.id (fromMaybe DVC.CAR req_.driverOnboardingVehicleCategory) False
            sendOperatorDeepLinkForAuth person driverMobile moc.merchantId moc.id operator

    sendDeepLinkForAuth :: DP.Person -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DP.Person -> Flow ()
    sendDeepLinkForAuth person mobileNumber merchantId merchantOpCityId fleetOwner = do
      let countryCode = fromMaybe "+91" person.mobileCountryCode
          phoneNumber = countryCode <> mobileNumber
      smsCfg <- asks (.smsCfg)
      withLogTag ("sending Deeplink Auth SMS" <> getId person.id) $ do
        (mbSender, message) <-
          MessageBuilder.buildFleetDeepLinkAuthMessage merchantOpCityId $
            MessageBuilder.BuildFleetDeepLinkAuthMessage
              { fleetOwnerName = fleetOwner.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult

    sendOperatorDeepLinkForAuth :: DP.Person -> Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DP.Person -> Flow ()
    sendOperatorDeepLinkForAuth person mobileNumber merchantId merchantOpCityId operator = do
      let countryCode = fromMaybe "+91" person.mobileCountryCode
          phoneNumber = countryCode <> mobileNumber
      smsCfg <- asks (.smsCfg)
      withLogTag ("sending Operator Deeplink Auth SMS" <> getId person.id) $ do
        (mbSender, message) <-
          MessageBuilder.buildOperatorDeepLinkAuthMessage merchantOpCityId $
            MessageBuilder.BuildOperatorDeepLinkAuthMessage
              { operatorName = operator.firstName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult

parseDriverInfo :: Int -> CreateDriversCSVRow -> Flow DriverDetails
parseDriverInfo idx row = do
  driverName <- Csv.cleanCSVField idx row.driverName "Driver name"
  driverPhoneNumber <- Csv.cleanCSVField idx row.driverPhoneNumber "Mobile number"
  let driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory = Csv.readMaybeCSVField idx (fromMaybe "" row.driverOnboardingVehicleCategory) "Onboarding Vehicle Category"
  let fleetPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.fleetPhoneNo) "Fleet number"
  pure $ DriverDetails driverName driverPhoneNumber driverOnboardingVehicleCategory fleetPhoneNo

fetchOrCreatePerson :: DMOC.MerchantOperatingCity -> DriverDetails -> Flow (DP.Person, Bool)
fetchOrCreatePerson moc req_ = do
  let authData =
        DReg.AuthReq
          { mobileNumber = Just req_.driverPhoneNumber,
            mobileCountryCode = Just "+91",
            merchantId = moc.merchantId.getId,
            merchantOperatingCity = Just moc.city,
            email = Nothing,
            name = Just req_.driverName,
            identifierType = Just DP.MOBILENUMBER,
            registrationLat = Nothing,
            registrationLon = Nothing
          }
  mobileNumberHash <- getDbHash req_.driverPhoneNumber
  QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash moc.merchantId DP.DRIVER
    >>= \case
      Nothing -> do
        person <- DReg.createDriverWithDetails authData Nothing Nothing Nothing Nothing Nothing moc.merchantId moc.id True
        let isNew = True in pure (person, isNew)
      Just person -> do
        let isNew = False in pure (person, isNew)

---------------------------------------------------------------------
postDriverDashboardFleetTrackDriver :: ShortId DM.Merchant -> Context.City -> Text -> Common.TrackDriverLocationsReq -> Flow Common.TrackDriverLocationsRes
postDriverDashboardFleetTrackDriver _ _ fleetOwnerId req = do
  void $
    mapM
      ( \driverId ->
          WMB.checkFleetDriverAssociation (cast driverId) (Id fleetOwnerId) >>= \isAssociated -> unless isAssociated (throwError $ DriverNotLinkedToFleet driverId.getId)
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
  nearbyDriverLocations <- LF.nearBy req.point.lat req.point.lon Nothing (Just [DV.BUS_NON_AC, DV.BUS_AC]) req.radius merchant.id (Just fleetOwnerId)
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
                          { driverName = driverName',
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

getDriverFleetAccessList :: ShortId DM.Merchant -> Context.City -> Flow Common.FleetOwnerListRes
getDriverFleetAccessList _ _ = throwError $ InternalError "Unimplemented!"

postDriverFleetAccessSelect :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetAccessSelect _ _ _ _ _ = throwError $ InternalError "Unimplemented!"

postDriverFleetV2AccessSelect :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetV2AccessSelect _ _ _ _ _ _ = throwError $ InternalError "Unimplemented!"

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
