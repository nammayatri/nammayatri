{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Driver
  ( postDriverFleetAddVehicle,
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
    getDriverFleetRoutes,
    getDriverFleetPossibleRoutes,
    postDriverFleetTripPlanner,
    getDriverFleetTripTransactions,
    postDriverFleetAddDriverBusRouteMapping,
    postDriverDashboardFleetTrackDriver,
    getDriverFleetWmbRouteDetails,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List (groupBy, sortOn)
import Data.List.NonEmpty (fromList, toList)
import Data.List.Split (chunksOf)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Data.Vector as V
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration as DRBReg
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.FleetDriverAssociation as FDA
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Action.UI.WMB as DWMB
import qualified Domain.Types.ApprovalRequest as DTR
import qualified Domain.Types.Common as DrInfo
import qualified Domain.Types.DriverLocation as DDL
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.FleetDriverAssociation
import qualified Domain.Types.FleetDriverAssociation as DTFDA
import Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as DRoute
import Domain.Types.TripTransaction
import qualified Domain.Types.TripTransaction as DTT
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleRouteMapping as DVRM
import qualified Domain.Types.VehicleVariant as DV
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude ((<|>))
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
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
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.WMB as WMB
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.Ride as CQRide
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner)
import qualified Storage.Queries.ApprovalRequest as QDR
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverPanCard as DPC
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverRCAssociationExtra as DRCAE
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.FleetOwnerInformation as FOI
import Storage.Queries.FleetRCAssociationExtra as FRAE
import qualified Storage.Queries.FleetRouteAssociation as QFRA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteTripStopMapping as QRTSM
import qualified Storage.Queries.TripTransaction as QTT
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import qualified Storage.Queries.VehicleRouteMapping as VRM
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
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity reqDriverPhoneNo fleetOwnerId mbMobileCountryCode req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  phoneNumberHash <- getDbHash reqDriverPhoneNo
  let mobileCountryCode = fromMaybe DCommon.mobileIndianCode mbMobileCountryCode
  driver <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneNumberHash merchant.id DP.DRIVER >>= fromMaybeM (DriverNotFound reqDriverPhoneNo)
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist driver.id.getId)
  rc <- RCQuery.findLastVehicleRCWrapper req.registrationNo
  whenJust rc $ \rcert -> checkRCAssociationForFleet fleetOwnerId rcert
  isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId driver.id fleetOwnerId True
  case isFleetDriver of
    Nothing -> throwError (DriverNotInFleet driver.id.getId fleetOwnerId)
    Just fleetDriver -> do
      unless fleetDriver.isActive $ throwError (DriverNotActiveInFleet driver.id.getId fleetDriver.id.getId)
  Redis.set (DomainRC.makeFleetOwnerKey req.registrationNo) fleetOwnerId -- setting this value here , so while creation of creation of vehicle we can add fleet owner id
  void $ DCommon.runVerifyRCFlow driver.id merchant merchantOpCityId opCity req True
  logTagInfo "dashboard -> addVehicle : " (show driver.id)
  pure Success

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
  Maybe Common.RequestStatus ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.DriverRequestResp
getDriverFleetGetDriverRequests merchantShortId opCity fleetOwnerId mbFrom mbTo mbStatus mbLimit mbOffset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let status_ = case mbStatus of
        Nothing -> Nothing
        Just status -> Just $ castReqStatus status
  driverRequests <- B.runInReplica $ QDR.findDriverRequestsByFleetOwnerId merchantOpCityId fleetOwnerId mbFrom mbTo status_ mbLimit mbOffset
  let driverRequestList = map buildDriverRequestListItem driverRequests
  pure $ Common.DriverRequestResp driverRequestList
  where
    buildDriverRequestListItem DTR.ApprovalRequest {..} =
      Common.DriverRequestDetails
        { raisedAt = createdAt,
          requestData = castCommonReqData requestData,
          status = castStatusToCommon (Just status),
          approvalRequestId = id.getId,
          ..
        }

castCommonReqData :: DTR.ApprovalRequestData -> Common.ApprovalRequestData
castCommonReqData (DTR.EndRide DTR.EndRideData {..}) = Common.EndRide $ Common.EndRideData {Common.tripTransactionId = Id tripTransactionId, ..}
castCommonReqData (DTR.ChangeRoute EmptyDynamicParam) = Common.ChangeRoute EmptyDynamicParam

castReqStatus :: Common.RequestStatus -> DTR.RequestStatus
castReqStatus = \case
  Common.AWAITING_APPROVAL -> DTR.AWAITING_APPROVAL
  Common.ACCEPTED -> DTR.ACCEPTED
  Common.REJECTED -> DTR.REJECTED
  Common.REVOKED -> DTR.REVOKED

castStatusToCommon :: Maybe DTR.RequestStatus -> Maybe Common.RequestStatus
castStatusToCommon = \case
  Just DTR.AWAITING_APPROVAL -> Just Common.AWAITING_APPROVAL
  Just DTR.ACCEPTED -> Just Common.ACCEPTED
  Just DTR.REJECTED -> Just Common.REJECTED
  Just DTR.REVOKED -> Just Common.REVOKED
  _ -> Nothing

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
    driverRequest <- B.runInReplica $ QDR.findByPrimaryKey (Id req.approvalRequestId) >>= fromMaybeM (DriverRequestNotFound req.approvalRequestId)
    driver <- B.runInReplica $ QPerson.findById driverRequest.requestorId >>= fromMaybeM (PersonDoesNotExist driverRequest.requestorId.getId)
    case driverRequest.status of
      DTR.AWAITING_APPROVAL -> QDR.updateStatusWithReason (castReqStatus req.status) (Just req.reason) (Id req.approvalRequestId)
      _ -> throwError $ RequestAlreadyProcessed driverRequest.id.getId
    void $ case req.status of
      Common.REJECTED -> Notification.requestRejectionNotification merchantOpCityId notificationTitle (message driverRequest) driver driver.deviceToken driverRequest{status = DTR.REJECTED, reason = Just req.reason}
      Common.ACCEPTED -> do
        case driverRequest.requestData of
          DTR.EndRide DTR.EndRideData {..} -> do
            fleetConfig <- QFC.findByPrimaryKey (Id fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetOwnerId)
            tripTransaction <- QTT.findByTransactionId (Id tripTransactionId) >>= fromMaybeM (TripTransactionNotFound tripTransactionId)
            void $ WMB.endTripTransaction fleetConfig tripTransaction (LatLong lat lon) DriverOnApproval
          _ -> pure ()
      _ -> pure ()
  pure Success
  where
    notificationTitle = "Request Rejected!"
    message req_ =
      cs $
        unwords
          [ "Sorry, your request for",
            (show $ getRequestType req_.requestData) <> " has been rejected",
            "Check the app for more details."
          ]
    getRequestType = \case
      DTR.EndRide _ -> DTR.END_RIDE
      DTR.ChangeRoute _ -> DTR.CHANGE_ROUTE

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
  Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle merchantShortId _ fleetOwnerId mbLimit mbOffset mbRegNumberString = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  mbRegNumberStringHash <- mapM getDbHash mbRegNumberString
  logDebug $ "reg number hash: " <> show mbRegNumberStringHash <> " param-string: " <> show mbRegNumberString
  merchant <- findMerchantByShortId merchantShortId
  vehicleList <- RCQuery.findAllByFleetOwnerIdAndSearchString (toInteger limit) (toInteger offset) merchant.id fleetOwnerId mbRegNumberStringHash
  vehicles <- traverse convertToVehicleAPIEntity vehicleList
  return $ Common.ListVehicleRes vehicles

convertToVehicleAPIEntity :: EncFlow m r => VehicleRegistrationCertificate -> m Common.VehicleAPIEntity
convertToVehicleAPIEntity VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  pure
    Common.VehicleAPIEntity
      { variant = DCommon.castVehicleVariantDashboard vehicleVariant,
        model = vehicleModel,
        color = vehicleColor,
        registrationNo = certificateNumber'
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
  Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver _merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbMobileNumber mbName = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  mbMobileNumberHash <- mapM getDbHash mbMobileNumber
  logDebug $ "mobile number hash: " <> show mbMobileNumberHash <> " param-string: " <> show mbMobileNumber
  driverList <- FDV.findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset mbMobileNumberHash mbName
  logDebug $ "driver list for fleet: " <> show driverList
  let driverIdList :: [Id DP.Person] = map DTFDA.driverId driverList
  driversInfo <- QPerson.getDriversByIdIn driverIdList
  fleetDriversInfos <- mapM convertToDriverAPIEntity driversInfo
  return $ Common.FleetListDriverRes fleetDriversInfos

convertToDriverAPIEntity :: DP.Person -> Flow Common.FleetDriversAPIEntity
convertToDriverAPIEntity DP.Person {..} = do
  unencryptedMobileNumber <- mapM decrypt mobileNumber
  pure $
    Common.FleetDriversAPIEntity
      { driverId = cast @DP.Person @Common.Driver id,
        firstName = firstName,
        middleName = middleName,
        lastName = lastName,
        mobileNumber = unencryptedMobileNumber,
        mobileCountryCode = mobileCountryCode
      }

---------------------------------------------------------------------
postDriverFleetUnlink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverFleetUnlink merchantShortId _opCity fleetOwnerId reqDriverId vehicleNo = do
  merchant <- findMerchantByShortId merchantShortId
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId personId fleetOwnerId True
  case isFleetDriver of
    Nothing -> throwError DriverNotPartOfFleet
    Just fleetDriver -> do
      unless fleetDriver.isActive $ throwError DriverNotActiveWithFleet
  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)
  driverInfo <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  DomainRC.deactivateCurrentRC personId
  QVehicle.deleteById personId
  when (driverInfo.onboardingVehicleCategory /= Just DVC.BUS) $ QDriverInfo.updateEnabledVerifiedState driverId False (Just False) -- TODO :: Is it required for Normal Fleet ?
  rc <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (RCNotFound vehicleNo)
  _ <- QRCAssociation.endAssociationForRC personId rc.id
  logTagInfo "fleet -> unlinkVehicle : " (show personId)
  pure Success

---------------------------------------------------------------------
postDriverFleetRemoveVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
postDriverFleetRemoveVehicle _merchantShortId _ fleetOwnerId_ vehicleNo = do
  vehicle <- QVehicle.findByRegistrationNo vehicleNo
  whenJust vehicle $ \veh -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId veh.driverId fleetOwnerId_ True
    when (isJust isFleetDriver) $ throwError (VehicleLinkedToAnotherDriver vehicleNo)
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
  fleetOwnerId <- case req.fleetOwnerId of
    Nothing -> throwError FleetOwnerIdRequired
    Just id -> pure id
  rcReq <- readCsv req.file merchantOpCity
  when (length rcReq > 100) $ throwError $ MaxVehiclesLimitExceeded 100 -- TODO: Configure the limit
  unprocessedVehicleRouteMappingEntities <-
    foldlM
      ( \unprocessedEntities (registerRcReq, vehicleNumberHash, routes) -> do
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
      ( \unprocessedEntities (registerRcReq, _, _) -> do
          try @_ @SomeException
            (postDriverFleetAddRCWithoutDriver merchantShortId opCity fleetOwnerId registerRcReq)
            >>= \case
              Left err -> return $ unprocessedEntities <> ["Unable to add Vehicle (" <> registerRcReq.vehicleRegistrationCertNumber <> "): " <> (T.pack $ displayException err)]
              Right _ -> return unprocessedEntities
      )
      []
      rcReq
  pure Common.APISuccessWithUnprocessedEntities {unprocessedEntities = unprocessedVehicleRouteMappingEntities <> unprocessedRCAdditionEntities}
  where
    readCsv csvFile merchantOpCity = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector VehicleDetailsCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM (parseVehicleInfo merchantOpCity) v >>= (pure . V.toList)

    parseVehicleInfo :: DMOC.MerchantOperatingCity -> Int -> VehicleDetailsCSVRow -> Flow (Common.RegisterRCReq, DbHash, [DRoute.Route])
    parseVehicleInfo moc idx row = do
      let airConditioned :: (Maybe Bool) = readMaybeCSVField idx row.airConditioned "Air Conditioned"
          mbRouteCodes :: Maybe [Text] = readMaybeCSVField idx row.routeCodes "Route Codes"
      vehicleCategory :: DVC.VehicleCategory <- readCSVField idx row.vehicleCategory "Vehicle Category"
      vehicleRegistrationCertNumber <- cleanCSVField idx row.registrationNo "Registration No"
      vehicleNumberHash <- getDbHash vehicleRegistrationCertNumber
      routes <-
        case mbRouteCodes of
          Just routeCodes -> mapM (\routeCode -> QRoute.findByRouteCode routeCode >>= fromMaybeM (RouteNotFound routeCode)) routeCodes
          Nothing -> pure []
      pure (Common.RegisterRCReq {dateOfRegistration = Nothing, multipleRC = Nothing, oxygen = Nothing, ventilator = Nothing, operatingCity = show moc.city, imageId = Id "bulkVehicleUpload", vehicleDetails = Nothing, vehicleCategory = Just vehicleCategory, ..}, vehicleNumberHash, routes)

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

    readCSVField :: Read a => Int -> Text -> Text -> Flow a
    readCSVField idx fieldValue fieldName =
      cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

data VehicleDetailsCSVRow = VehicleDetailsCSVRow
  { registrationNo :: Text,
    airConditioned :: Text,
    routeCodes :: Text,
    vehicleCategory :: Text
  }
  deriving (Show)

instance FromNamedRecord VehicleDetailsCSVRow where
  parseNamedRecord r =
    VehicleDetailsCSVRow
      <$> r .: "registration_no"
      <*> r .: "air_conditioned"
      <*> r .: "route_codes"
      <*> r .: "vehicle_category"

---------------------------------------------------------------------
postDriverFleetRemoveDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Flow APISuccess
postDriverFleetRemoveDriver _merchantShortId _ fleetOwnerId driverId = do
  let personId = cast @Common.Driver @DP.Person driverId
  associationList <- QRCAssociation.findAllLinkedByDriverId personId
  forM_ associationList $ \assoc -> do
    rc <- RCQuery.findByRCIdAndFleetOwnerId assoc.rcId $ Just fleetOwnerId
    when (isJust rc) $ throwError (InvalidRequest "Driver is linked to fleet Vehicle, first unlink then try")
  FDV.endFleetDriverAssociation fleetOwnerId personId
  pure Success

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
  listOfAllRc <- getListOfVehicles mbVehicleNumber fleetOwnerId mbLimit mbOffset Nothing merchant.id
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

getListOfVehicles :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Common.FleetVehicleStatus -> Id DM.Merchant -> Flow [VehicleRegistrationCertificate]
getListOfVehicles mbVehicleNo fleetOwnerId mbLimit mbOffset mbStatus merchantId = do
  case mbVehicleNo of
    Just vehicleNo -> do
      vehicleInfo <- RCQuery.findLastVehicleRCFleet' vehicleNo fleetOwnerId
      pure $ maybeToList vehicleInfo
    Nothing -> do
      let limit = fromIntegral $ min 10 $ fromMaybe 5 mbLimit
          offset = fromIntegral $ fromMaybe 0 mbOffset
      case mbStatus of
        Just Common.InActive -> RCQuery.findAllInactiveRCForFleet fleetOwnerId limit offset merchantId
        Just Common.Pending -> RCQuery.findAllRCByStatusForFleet fleetOwnerId (castFleetVehicleStatus mbStatus) Nothing limit offset merchantId
        Just Common.Invalid -> RCQuery.findAllRCByStatusForFleet fleetOwnerId (castFleetVehicleStatus mbStatus) Nothing limit offset merchantId
        Just Common.Active -> RCQuery.findAllRCByStatusForFleet fleetOwnerId (castFleetVehicleStatus mbStatus) (Just True) limit offset merchantId
        Nothing -> RCQuery.findAllByFleetOwnerId (Just $ fromInteger limit) (Just $ fromInteger offset) (Just fleetOwnerId)

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
  let driverList = mapMaybe (.driverId') driverStatsList
  driverListWithInfo <- QPerson.findAllPersonAndDriverInfoWithDriverIds driverList
  res <- forM (zip driverListWithInfo driverStatsList) $ \((driver, driverInfo'), driverStats) -> do
    let driverName = driver.firstName <> " " <> fromMaybe "" driver.lastName
    mobileNumber <- mapM decrypt driver.mobileNumber
    let totalDuration = calculateTimeDifference driverStats.totalDuration
    pure $
      Common.FleetEarningRes
        { driverId = Just $ cast @DP.Person @Common.Driver driver.id,
          driverName = Just driverName,
          totalRides = driverStats.completedRides,
          totalEarning = driverStats.totalEarnings,
          vehicleNo = Nothing,
          status = Just $ castDriverStatus driverInfo'.mode,
          vehicleType = Nothing,
          totalDuration = totalDuration,
          distanceTravelled = fromIntegral driverStats.totalDistanceTravelled / 1000.0,
          driverPhoneNo = mobileNumber,
          cancelledRides = driverStats.cancelledRides
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
  listOfAllDrivers <- getListOfDrivers mbCountryCode mbPhoneNo fleetOwnerId merchant.id Nothing mbLimit mbOffset Nothing
  listOfAllVehicle <- getListOfVehicles mbVehicleNo fleetOwnerId mbLimit mbOffset Nothing merchant.id
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
                      ..
                    }
            pure listItem

getListOfDrivers :: Maybe Text -> Maybe Text -> Text -> Id DM.Merchant -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Common.DriverMode -> Flow [FleetDriverAssociation]
getListOfDrivers mbCountryCode mbDriverPhNo fleetOwnerId merchantId mbIsActive mbLimit mbOffset mbMode = do
  case mbDriverPhNo of
    Just driverPhNo -> do
      mobileNumberHash <- getDbHash driverPhNo
      let countryCode = fromMaybe "+91" mbCountryCode
      mbDriver <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchantId DP.DRIVER
      case mbDriver of
        Just driver -> do
          fleetDriverAssociation <- FDV.findByDriverIdAndFleetOwnerId driver.id fleetOwnerId True
          pure $ maybeToList fleetDriverAssociation
        Nothing -> pure $ []
    Nothing -> do
      let limit = min 10 $ fromMaybe 5 mbLimit
          offset = fromMaybe 0 mbOffset
      case mbMode of
        Just mode -> FDV.findAllDriversByFleetOwnerIdByMode fleetOwnerId (castDashboardDriverStatus mode) mbIsActive (fromIntegral limit) (fromIntegral offset)
        _ -> FDV.findAllDriverByFleetOwnerIdAndMbIsActive fleetOwnerId mbIsActive limit offset

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
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverAssociation merchantShortId _opCity fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbDriverPhNo mbStats mbFrom mbTo mbMode = do
  merchant <- findMerchantByShortId merchantShortId
  listOfAllDrivers <- getListOfDrivers mbCountryCode mbDriverPhNo fleetOwnerId merchant.id mbIsActive mbLimit mbOffset mbMode
  listItems <- createFleetDriverAssociationListItem listOfAllDrivers
  let summary = Common.Summary {totalCount = 10000, count = length listItems}
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = listItems, summary = summary}
  where
    createFleetDriverAssociationListItem :: [FleetDriverAssociation] -> Flow [Common.DriveVehicleAssociationListItem]
    createFleetDriverAssociationListItem fdaList = do
      let driverList = map (\fda -> fda.driverId) fdaList
      driverListWithInfo <- QPerson.findAllPersonAndDriverInfoWithDriverIds driverList
      now <- getCurrentTime
      let defaultFrom = UTCTime (utctDay now) 0
          from = fromMaybe defaultFrom mbFrom
          to = fromMaybe now mbTo
      forM (zip driverListWithInfo fdaList) $ \((driver, driverInfo'), fda) -> do
        driverRCAssociation <- QRCAssociation.findAllByDriverId driver.id
        let rcAssociatedWithFleet = filter (\(_, rc) -> rc.fleetOwnerId == Just fleetOwnerId) driverRCAssociation
        (vehicleNo, vehicleType) <- case rcAssociatedWithFleet of ---- so the logic is if it have active association with the fleet vehicle return that otherwise return the latest one
          [] -> pure (Nothing, Nothing)
          associations -> do
            let activeAssociation = find (\(assoc, _) -> assoc.isRcActive) associations
            case activeAssociation of
              Just (_, rc) -> getVehicleDetails rc ------- if driver is using fleet vehicle
              Nothing -> getVehicleDetails $ snd $ head associations -------- otherwise give the latest active association
        let driverName = Just driver.firstName
        driverPhoneNo <- mapM decrypt driver.mobileNumber
        driverLicenseStatus <- do
          mbDl <- B.runInReplica $ QDriverLicense.findByDriverId driver.id
          case mbDl of
            Just dl -> do
              let dlStatus = DCommon.castVerificationStatus dl.verificationStatus
              pure dlStatus
            Nothing -> pure Common.PENDING
        (completedRides, earning) <- case mbStats of
          Just True -> do
            rides <- CQRide.totalRidesByFleetOwnerPerDriver (Just fleetOwnerId) driver.id from to
            earnings <- CQRide.totalEarningsByFleetOwnerPerDriver (Just fleetOwnerId) driver.id from to
            pure (rides, earnings)
          _ -> pure (0, 0)
        let driverStatus = Just $ castDriverStatus driverInfo'.mode -- if isNothing vehicleNo then Nothing else Just $ castDriverStatus driverInfo'.mode
        isDriverOnRide <-
          if driverInfo'.onRide
            then do
              currentTripTransaction <- WMB.findNextEligibleTripTransactionByDriverIdStatus driver.id IN_PROGRESS
              return $ isJust currentTripTransaction
            else pure False
        let isRcAssociated = isJust vehicleNo
        let isDriverActive = fda.isActive
        let driverId = Just $ driver.id.getId
        let ls =
              Common.DriveVehicleAssociationListItem
                { vehicleNo = vehicleNo,
                  status = driverStatus,
                  isDriverActive = isDriverActive,
                  isDriverOnRide = Just isDriverOnRide,
                  verificationDocsStatus =
                    Just
                      Common.VerificationDocsStatus
                        { driverLicense = Just driverLicenseStatus,
                          vehicleRegistrationCertificate = Nothing,
                          vehicleFitness = Nothing,
                          vehiclePermit = Nothing,
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
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetVehicleAssociation merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbVehicleNumber mbIncludeStats mbFrom mbTo mbStatus = do
  merchant <- findMerchantByShortId merchantShortId
  listOfAllVehicle <- getListOfVehicles mbVehicleNumber fleetOwnerId mbLimit mbOffset mbStatus merchant.id
  listItems <- createFleetVehicleAssociationListItem listOfAllVehicle
  let summary = Common.Summary {totalCount = 10000, count = length listItems}
  pure $ Common.DrivertoVehicleAssociationRes {fleetOwnerId = fleetOwnerId, listItem = listItems, summary = summary}
  where
    createFleetVehicleAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => [VehicleRegistrationCertificate] -> m [Common.DriveVehicleAssociationListItem]
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
        (driverName, driverId, driverPhoneNo, driverStatus) <- case rcActiveAssociation of
          Just activeAssociation -> getFleetDriverInfo activeAssociation.driverId False ------- when vehicle is in active state
          Nothing -> do
            latestAssociation <- QRCAssociation.findLatestLinkedByRCId vrc.id now ------- when there is not any active association then i will find out the latest association  (vehicle is in inActive state)
            case latestAssociation of
              Just latestAssoc -> getFleetDriverInfo latestAssoc.driverId False
              Nothing -> pure (Nothing, Nothing, Nothing, Nothing) -------- when vehicle is unAssigned
        let vehicleType = DCommon.castVehicleVariantDashboard vrc.vehicleVariant
        let isDriverActive = isJust driverName -- Check if there is a current active driver
        let isRcAssociated = isJust rcActiveAssociation
        let verificationDocs =
              Common.VerificationDocsStatus
                { vehicleRegistrationCertificate = Just $ DCommon.castVerificationStatus vrc.verificationStatus,
                  vehiclePermit = Nothing, ------ currently we are not verifying these docs therefore
                  vehicleInsurance = Nothing,
                  vehicleFitness = Nothing,
                  driverLicense = Nothing
                }
        let ls =
              Common.DriveVehicleAssociationListItem
                { vehicleNo = Just decryptedVehicleRC,
                  status = Just $ castDriverStatus driverStatus,
                  isDriverOnRide = Nothing,
                  isDriverActive = isDriverActive,
                  earning = snd stats,
                  completedRides = fst stats,
                  vehicleType = vehicleType,
                  verificationDocsStatus = Just verificationDocs,
                  ..
                }
        pure ls

getFleetDriverInfo :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id DP.Person -> Bool -> m (Maybe Text, Maybe Text, Maybe Text, Maybe DrInfo.DriverMode)
getFleetDriverInfo driverId isDriver = do
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mode <-
    if isDriver
      then do
        driverInfo' <- QDriverInfo.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
        return (driverInfo'.mode)
      else return Nothing
  mobileNumber <- mapM decrypt driver.mobileNumber
  return (Just driver.firstName, Just driver.id.getId, mobileNumber, mode)

---------------------------------------------------------------------
postDriverFleetVehicleDriverRcStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.RCStatusReq ->
  Flow APISuccess
postDriverFleetVehicleDriverRcStatus merchantShortId opCity reqDriverId fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
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
  fleetOwnerInfo <- B.runInReplica $ FOI.findByPrimaryKey personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  fleetConfig <- QFC.findByPrimaryKey personId
  panDetails <- B.runInReplica $ DPC.findByDriverId personId
  panNumber <- case panDetails of
    Just pan -> Just <$> decrypt pan.panCardNumber
    Nothing -> pure Nothing
  makeFleetOwnerInfoRes panNumber fleetConfig fleetOwnerInfo
  where
    makeFleetOwnerInfoRes :: Maybe Text -> Maybe DFC.FleetConfig -> DFOI.FleetOwnerInformation -> Flow Common.FleetOwnerInfoRes
    makeFleetOwnerInfoRes panNumber mbFleetConfig DFOI.FleetOwnerInformation {..} = do
      let fleetConfig =
            mbFleetConfig <&> \fleetConfig' ->
              Common.FleetConfig
                { allowAutomaticRoundTripAssignment = fleetConfig'.allowAutomaticRoundTripAssignment,
                  allowEndingMidRoute = fleetConfig'.allowEndingMidRoute,
                  allowStartRideFromQR = fleetConfig'.allowStartRideFromQR,
                  endRideDistanceThreshold = fleetConfig'.endRideDistanceThreshold,
                  rideEndApproval = fleetConfig'.rideEndApproval
                }
      return $ Common.FleetOwnerInfoRes {panNumber = panNumber, fleetType = show fleetType, ..}

---------------------------------------------------------------------
postDriverFleetSendJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.AuthReq ->
  Flow Common.AuthRes
postDriverFleetSendJoiningOtp merchantShortId opCity fleetOwnerName req = do
  merchant <- findMerchantByShortId merchantShortId
  smsCfg <- asks (.smsCfg)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mobileNumberHash <- getDbHash req.mobileNumber
  mbPerson <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER
  case mbPerson of
    Nothing -> DRBReg.auth merchantShortId opCity req -------------- to onboard a driver that is not the part of the fleet
    Just person -> do
      let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
          phoneNumber = req.mobileCountryCode <> req.mobileNumber
      otpCode <- maybe generateOTPCode return useFakeOtpM
      withLogTag ("personId_" <> getId person.id) $ do
        (mbSender, message) <-
          MessageBuilder.buildFleetJoiningMessage merchantOpCityId $
            MessageBuilder.BuildFleetJoiningMessageReq
              { otp = otpCode,
                fleetOwnerName = fleetOwnerName
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
        let key = makeFleetDriverOtpKey phoneNumber
        Redis.setExp key otpCode 3600
      pure $ Common.AuthRes {authId = "ALREADY_USING_APPLICATION", attempts = 0}

---------------------------------------------------------------------
postDriverFleetVerifyJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Common.VerifyFleetJoiningOtpReq ->
  Flow APISuccess
postDriverFleetVerifyJoiningOtp merchantShortId opCity fleetOwnerId mbAuthId req = do
  merchant <- findMerchantByShortId merchantShortId
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound req.mobileNumber)
  case mbAuthId of
    Just authId -> do
      smsCfg <- asks (.smsCfg)
      merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
      fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
      deviceToken <- fromMaybeM (DeviceTokenNotFound) $ req.deviceToken
      void $ DRBReg.verify authId True fleetOwnerId Common.AuthVerifyReq {otp = req.otp, deviceToken = deviceToken}
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
      assoc <- FDA.makeFleetDriverAssociation person.id fleetOwnerId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QFDV.create assoc
  pure Success

makeFleetDriverOtpKey :: Text -> Text
makeFleetDriverOtpKey phoneNo = "Fleet:Driver:PhoneNo" <> phoneNo

---------------------------------------------------------------------
postDriverFleetLinkRCWithDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.LinkRCWithDriverForFleetReq ->
  Flow APISuccess
postDriverFleetLinkRCWithDriver merchantShortId opCity fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  phoneNumberHash <- getDbHash req.driverMobileNumber
  let mobileCountryCode = fromMaybe DCommon.mobileIndianCode req.driverMobileCountryCode
  driver <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneNumberHash merchant.id DP.DRIVER >>= fromMaybeM (DriverNotFound req.driverMobileNumber)
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist driver.id.getId)
  rc <- RCQuery.findLastVehicleRCWrapper req.vehicleRegistrationNumber >>= fromMaybeM (RCNotFound req.vehicleRegistrationNumber)
  when (isNothing rc.fleetOwnerId || (isJust rc.fleetOwnerId && rc.fleetOwnerId /= Just fleetOwnerId)) $ throwError VehicleNotPartOfFleet
  unless (rc.verificationStatus == Documents.VALID) $ throwError (RcNotValid)
  isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId driver.id fleetOwnerId True
  case isFleetDriver of
    Nothing -> throwError DriverNotPartOfFleet
    Just fleetDriver -> do
      unless fleetDriver.isActive $ throwError DriverNotActiveWithFleet
  now <- getCurrentTime
  mbAssoc <- QRCAssociation.findLinkedByRCIdAndDriverId driver.id rc.id now
  when (isNothing mbAssoc) $ do
    driverRCAssoc <- makeRCAssociation driver.merchantId driver.merchantOperatingCityId driver.id rc.id (convertTextToUTC (Just "2099-12-12"))
    QRCAssociation.create driverRCAssoc
  return Success

---------------------------------------------------------------------
postDriverDashboardFleetWmbTripEnd ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.TripTransaction ->
  Text ->
  Flow APISuccess
postDriverDashboardFleetWmbTripEnd _ _ tripTransactionId fleetOwnerId = do
  fleetConfig <- QFC.findByPrimaryKey (Id fleetOwnerId) >>= fromMaybeM (FleetConfigNotFound fleetOwnerId)
  tripTransaction <- QTT.findByTransactionId (cast tripTransactionId) >>= fromMaybeM (TripTransactionNotFound tripTransactionId.getId)
  currentDriverLocation <-
    try @_ @SomeException (LF.driversLocation [tripTransaction.driverId])
      >>= \case
        Left _ -> throwError $ InvalidRequest "Driver is not active since 24 hours, please ask driver to go online and then end the trip."
        Right locations -> listToMaybe locations & fromMaybeM (InvalidRequest "Driver is not active since 24 hours, please ask driver to go online and then end the trip.")
  void $ WMB.cancelTripTransaction fleetConfig tripTransaction (LatLong currentDriverLocation.lat currentDriverLocation.lon) Dashboard
  pure Success

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
              Maps.getDistances merchantId merchantOpCityId $
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
  vehicleRC <- WMB.linkVehicleToDriver (cast req.driverId) merchant.id merchantOpCity.id fleetOwnerId req.vehicleNumber
  createTripTransactions merchant.id merchantOpCity.id fleetOwnerId req.driverId vehicleRC req.trips
  pure Success

createTripTransactions :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Id Common.Driver -> VehicleRegistrationCertificate -> [Common.TripDetails] -> Flow ()
createTripTransactions merchantId merchantOpCityId fleetOwnerId driverId vehicleRC trips = do
  allTransactions <-
    foldM
      ( \accTransactions trip -> do
          transactions <- makeTripTransactions trip
          return $ accTransactions <> transactions
      )
      []
      trips
  WMB.findNextActiveTripTransaction (cast driverId)
    >>= \case
      Just _ -> QTT.createMany allTransactions
      Nothing -> do
        QTT.createMany allTransactions
        whenJust (listToMaybe allTransactions) $ \tripTransaction -> do
          route <- QRoute.findByRouteCode tripTransaction.routeCode >>= fromMaybeM (RouteNotFound tripTransaction.routeCode)
          (routeSourceStopInfo, routeDestinationStopInfo) <- WMB.getSourceAndDestinationStopInfo route route.code
          WMB.assignTripTransaction tripTransaction route True routeSourceStopInfo.point routeDestinationStopInfo.point True
  where
    makeTripTransactions :: Common.TripDetails -> Flow [DTT.TripTransaction]
    makeTripTransactions trip = do
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
                tripTransaction <- mkTransaction routeCode (Just roundRouteCode_) endStopCode
                pure $ accTransactions <> [tripTransaction]
            )
            []
            [0 .. (2 * roundTrip.frequency) -1]
        (_, _) -> do
          tripTransaction <- mkTransaction route.code route.roundRouteCode routeDestinationStopInfo.code
          pure [tripTransaction]

    mkTransaction :: Text -> Maybe Text -> Text -> Flow DTT.TripTransaction
    mkTransaction routeCode roundRouteCode endStopCode = do
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
            status = DTT.TRIP_ASSIGNED,
            routeCode = routeCode,
            roundRouteCode = roundRouteCode,
            tripCode = Nothing,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = maybe DrInfo.BUS_NON_AC DV.castVariantToServiceTier vehicleRC.vehicleVariant,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            tripTerminationSource = Nothing,
            endRideApprovalRequestId = Nothing,
            createdAt = now,
            updatedAt = now
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
getDriverFleetTripTransactions merchantShortId opCity _ driverId mbFrom mbTo mbVehicleNumber limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  _ <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  tripTransactions <- QTT.findAllTripTransactionByDriverIdWithinCreationRange (Just limit) (Just offset) (cast driverId) mbFrom mbTo mbVehicleNumber
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

---------------------------------------------------------------------
data CreateDriverBusRouteMappingCSVRow = CreateDriverBusRouteMappingCSVRow
  { driverId :: Text,
    vehicleNumber :: Text,
    routeCode :: Text,
    roundTripFreq :: Text
  }

data DriverBusRouteDetails = DriverBusRouteDetails
  { driverPhoneNo :: Text,
    vehicleNumber :: Text,
    routeCode :: Text,
    roundTripFreq :: Maybe Int
  }

instance FromNamedRecord CreateDriverBusRouteMappingCSVRow where
  parseNamedRecord r =
    CreateDriverBusRouteMappingCSVRow
      <$> r .: "driver_phone_number"
      <*> r .: "vehicle_number"
      <*> r .: "route_code"
      <*> r .: "round_trip_freq"

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
  driverBusRouteDetails <- readCsv req.file

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
                return (driver, driverGroup.driverPhoneNo, vehicleNumber, tripPlannerRequests)
      )
      groupedDetails

  unprocessedEntities <-
    foldlM
      ( \unprocessedEntities (driver, driverMobileNumber, vehicleNumber, tripPlannerRequests) -> do
          try @_ @SomeException
            (WMB.linkVehicleToDriver (cast driver.id) merchant.id merchantOpCity.id fleetOwnerId vehicleNumber)
            >>= \case
              Left err -> return $ unprocessedEntities <> ["Unable to link vehicle to the Driver (" <> driverMobileNumber <> "): " <> (T.pack $ displayException err)]
              Right vehicleRC -> do
                try @_ @SomeException
                  (createTripTransactions merchant.id merchantOpCity.id fleetOwnerId (cast driver.id) vehicleRC tripPlannerRequests)
                  >>= \case
                    Left err -> do
                      WMB.unlinkVehicleToDriver (cast driver.id) merchant.id merchantOpCity.id vehicleNumber
                      return $ unprocessedEntities <> ["Unable to create Trip Transactions for Driver (" <> driverMobileNumber <> "): " <> (T.pack $ displayException err)]
                    Right _ -> return unprocessedEntities
      )
      []
      driverTripPlanner
  pure $ Common.APISuccessWithUnprocessedEntities unprocessedEntities
  where
    readCsv csvFile = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CreateDriverBusRouteMappingCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM parseMappingInfo v >>= (pure . V.toList)

    parseMappingInfo :: Int -> CreateDriverBusRouteMappingCSVRow -> Flow DriverBusRouteDetails
    parseMappingInfo idx row = do
      driverPhoneNo <- cleanCSVField idx row.driverId "Driver Phone number"
      vehicleNumber <- cleanCSVField idx row.vehicleNumber "Vehicle number"
      routeCode <- cleanCSVField idx row.routeCode "Route code"
      let roundTripFreq = readMaybeCSVField idx row.roundTripFreq "Round trip freq"
      pure $ DriverBusRouteDetails driverPhoneNo vehicleNumber routeCode roundTripFreq

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    makeTripPlannerReq driverGroup =
      Common.TripDetails
        { routeCode = driverGroup.routeCode,
          roundTrip = fmap (\freq -> Common.RoundTripDetail {frequency = freq}) driverGroup.roundTripFreq
        }

readMaybeCSVField :: Read a => Int -> Text -> Text -> Maybe a
readMaybeCSVField _ fieldValue _ = cleanField fieldValue >>= readMaybe . T.unpack

cleanField :: Text -> Maybe Text
cleanField = replaceEmpty . T.strip

replaceEmpty :: Text -> Maybe Text
replaceEmpty = \case
  "" -> Nothing
  "no constraint" -> Nothing
  "no_constraint" -> Nothing
  x -> Just x

---------------------------------------------------------------------
data CreateDriversCSVRow = CreateDriversCSVRow
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Text
  }

data DriverDetails = DriverDetails
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: DVC.VehicleCategory
  }

instance FromNamedRecord CreateDriversCSVRow where
  parseNamedRecord r =
    CreateDriversCSVRow
      <$> r .: "driver_name"
      <*> r .: "driver_phone_number"
      <*> r .: "driver_onboarding_vehicle_category"

postDriverFleetAddDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreateDriversReq ->
  Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDrivers merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  fleetOwnerId <- case req.fleetOwnerId of
    Nothing -> throwError FleetOwnerIdRequired
    Just id -> pure id
  driverDetails <- readCsv req.file
  when (length driverDetails > 100) $ throwError $ MaxDriversLimitExceeded 100 -- TODO: Configure the limit
  unprocessedEntities <-
    foldlM
      ( \unprocessedEntities driverDetail -> do
          try @_ @SomeException
            (processDriver merchantOpCity fleetOwnerId driverDetail)
            >>= \case
              Left err -> return $ unprocessedEntities <> ["Unable to add Driver (" <> driverDetail.driverPhoneNumber <> ") to the Fleet: " <> (T.pack $ displayException err)]
              Right _ -> return unprocessedEntities
      )
      []
      driverDetails
  pure $ Common.APISuccessWithUnprocessedEntities unprocessedEntities
  where
    processDriver :: DMOC.MerchantOperatingCity -> Text -> DriverDetails -> Flow () -- TODO: create single query to update all later
    processDriver moc fleetOwnerId req_ = do
      let driverMobile = req_.driverPhoneNumber
          authData =
            DReg.AuthReq
              { mobileNumber = Just req_.driverPhoneNumber,
                mobileCountryCode = Just "+91",
                merchantId = moc.merchantId.getId,
                merchantOperatingCity = Just opCity,
                email = Nothing,
                name = Just req_.driverName,
                identifierType = Just DP.MOBILENUMBER,
                registrationLat = Nothing,
                registrationLon = Nothing
              }
      mobileNumberHash <- getDbHash req_.driverPhoneNumber
      person <-
        QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash moc.merchantId DP.DRIVER
          >>= maybe (DReg.createDriverWithDetails authData Nothing Nothing Nothing Nothing Nothing moc.merchantId moc.id True) return
      WMB.checkFleetDriverAssociation person.id (Id fleetOwnerId)
        >>= \isAssociated -> unless isAssociated $ do
          fork "Sending Fleet Consent SMS to Driver" $ do
            fleetOwner <- QPerson.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
            FDV.createFleetDriverAssociationIfNotExists person.id (Id fleetOwnerId) req_.driverOnboardingVehicleCategory False
            sendDeepLinkForAuth person driverMobile moc.merchantId moc.id fleetOwner

    readCsv csvFile = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CreateDriversCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM parseDriverInfo v >>= (pure . V.toList)

    parseDriverInfo :: Int -> CreateDriversCSVRow -> Flow DriverDetails
    parseDriverInfo idx row = do
      driverName <- cleanCSVField idx row.driverName "Driver name"
      driverPhoneNumber <- cleanCSVField idx row.driverPhoneNumber "Mobile number"
      driverOnboardingVehicleCategory :: DVC.VehicleCategory <- readCSVField idx row.driverOnboardingVehicleCategory "Onboarding Vehicle Category"
      pure $ DriverDetails driverName driverPhoneNumber driverOnboardingVehicleCategory

    readCSVField :: Read a => Int -> Text -> Text -> Flow a
    readCSVField idx fieldValue fieldName =
      cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

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
