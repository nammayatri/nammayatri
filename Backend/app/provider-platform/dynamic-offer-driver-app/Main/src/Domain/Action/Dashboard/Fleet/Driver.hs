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
    postDriverFleetVehicleDriverRCstatus,
    postDriverUpdateFleetOwnerInfo,
    getDriverFleetOwnerInfo,
    postDriverFleetSendJoiningOtp,
    postDriverFleetVerifyJoiningOtp,
    postDriverFleetLinkRCWithDriver,
  )
where

import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as DC
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Common
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Management.DriverRegistration as DReg
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.FleetDriverAssociation as FDA
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Types.DriverInformation as DrInfo
import qualified Domain.Types.DriverPlan as DDPlan
import Domain.Types.FleetDriverAssociation
import qualified Domain.Types.FleetDriverAssociation as DTFDA
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as DP
import Domain.Types.Plan
import Domain.Types.VehicleRegistrationCertificate
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude
import Kernel.Sms.Config
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import SharedLogic.DriverOnboarding
import qualified SharedLogic.EventTracking as SEVT
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.Clickhouse.Ride as CQRide
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverPanCard as DPC
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverRCAssociationExtra as DRCAE
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.FleetOwnerInformation as FOI
import Storage.Queries.FleetRCAssociationExtra as FRAE
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Error
import Tools.SMS as Sms hiding (Success)

-- TODO Domain.Action.Dashboard.Fleet.Operations

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
    Nothing -> throwError (InvalidRequest "Driver is not part of this fleet, add this driver to the fleet before adding a vehicle with them")
    Just fleetDriver -> do
      unless fleetDriver.isActive $ throwError (InvalidRequest "Driver is not active with this fleet, add this driver to the fleet before adding a vehicle with them")
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
    when (isNothing isFleetDriver) $ throwError (InvalidRequest "Vehicle is associated with a driver who is not part of this fleet, First Unlink the vehicle from that driver and then try again")

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
            vehicleDetails = Nothing,
            vehicleCategory = Nothing
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
  Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle _ _ fleetOwnerId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  vehicleList <- RCQuery.findAllByFleetOwnerId (Just limit) (Just offset) (Just fleetOwnerId)
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
  Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver _merchantShortId _opCity fleetOwnerId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  driverList <- FDV.findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset
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
  DomainRC.deactivateCurrentRC personId
  QVehicle.deleteById personId
  QDriverInfo.updateEnabledVerifiedState driverId False (Just False)
  rc <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (RCNotFound vehicleNo)
  _ <- QRCAssociation.endAssociationForRC personId rc.id
  void $ toggleDriverSubscriptionByService (personId, driver.merchantId, driver.merchantOperatingCityId) YATRI_RENTAL Nothing False vehicleNo
  logTagInfo "fleet -> unlinkVehicle : " (show personId)
  pure Success

toggleDriverSubscriptionByService ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe (Id Plan) ->
  Bool ->
  Text ->
  Flow ()
toggleDriverSubscriptionByService (driverId, mId, mOpCityId) serviceName mbPlanToAssign toToggle vehicleNo = do
  (autoPayStatus, driverPlan) <- DTPlan.getSubcriptionStatusWithPlan serviceName driverId
  transporterConfig <- CTC.findByMerchantOpCityId mOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
  if toToggle
    then do
      planToAssign <- getPlanId mbPlanToAssign
      case autoPayStatus of
        Just DrInfo.ACTIVE -> pure ()
        _ -> callSubscribeFlowForDriver planToAssign
      QDP.updatesubscriptionServiceRelatedDataInDriverPlan driverId (DDPlan.RentedVehicleNumber vehicleNo) serviceName
      QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
      fork "notify rental event" $ do
        DCommon.notifyYatriRentalEventsToDriver vehicleNo WHATSAPP_VEHICLE_LINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
    else do
      let vehicleLinkedWithDPlan = case driverPlan <&> (.subscriptionServiceRelatedData) of
            Just (DDPlan.RentedVehicleNumber vNo) -> Just vNo
            _ -> Nothing
      when (isJust driverPlan && vehicleLinkedWithDPlan == Just vehicleNo) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
        fork "track service toggle" $ do
          case driverPlan of
            Just dp -> SEVT.trackServiceUsageChargeToggle dp Nothing
            Nothing -> pure ()
        fork "notify rental event" $ do
          DCommon.notifyYatriRentalEventsToDriver vehicleNo WHATSAPP_VEHICLE_UNLINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
  where
    getPlanId :: Maybe (Id Plan) -> Flow (Id Plan)
    getPlanId mbPlanId = do
      case mbPlanId of
        Nothing -> do
          plans <- CQP.findByMerchantOpCityIdAndTypeWithServiceName mOpCityId DEFAULT serviceName
          case plans of
            [] -> throwError $ InternalError "No default plans found"
            [pl] -> pure pl.id
            _ -> throwError $ InternalError "Multiple default plans found"
        Just planId -> pure planId
    callSubscribeFlowForDriver :: Id Plan -> Flow ()
    callSubscribeFlowForDriver planId = do
      driverInfo' <- QDriverInfo.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
      let serviceSpecificData = DDPlan.RentedVehicleNumber vehicleNo
      _ <- DTPlan.planSubscribe serviceName planId (True, Just WHATSAPP) (cast driverId, mId, mOpCityId) driverInfo' serviceSpecificData
      pure ()

---------------------------------------------------------------------
postDriverFleetRemoveVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
postDriverFleetRemoveVehicle _merchantShortId opCity fleetOwnerId_ vehicleNo = do
  vehicle <- QVehicle.findByRegistrationNo vehicleNo
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  whenJust vehicle $ \veh -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId veh.driverId fleetOwnerId_ True
    when (isJust isFleetDriver) $ throwError (InvalidRequest "Vehicle is linked to fleet driver , first unlink then try")
  vehicleRC <- RCQuery.findLastVehicleRCWrapper vehicleNo >>= fromMaybeM (VehicleDoesNotExist vehicleNo)
  unless (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId == Just fleetOwnerId_) $ throwError (FleetOwnerVehicleMismatchError fleetOwnerId_)
  associations <- QRCAssociation.findAllActiveAssociationByRCId vehicleRC.id ----- Here ending all the association of the vehicle with the fleet drivers
  forM_ associations $ \assoc -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId assoc.driverId fleetOwnerId_ True
    when (isJust isFleetDriver) $ QRCAssociation.endAssociationForRC assoc.driverId vehicleRC.id
  RCQuery.upsert (updatedVehicleRegistrationCertificate vehicleRC)
  FRAE.endAssociationForRC (Id fleetOwnerId_ :: Id DP.Person) vehicleRC.id
  case vehicle <&> (.driverId) of
    Just driverId -> do
      void $ toggleDriverSubscriptionByService (driverId, merchant.id, merchantOpCityId) YATRI_RENTAL Nothing False vehicleNo
    Nothing -> pure ()
  pure Success
  where
    updatedVehicleRegistrationCertificate VehicleRegistrationCertificate {..} = VehicleRegistrationCertificate {fleetOwnerId = Nothing, ..}

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
    when (isJust rc) $ throwError (InvalidRequest "Driver is linked to fleet Vehicle , first unlink then try")
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

calculateTimeDifference :: Int -> DC.TotalDuration
calculateTimeDifference diffTime = DC.TotalDuration {..}
  where
    diffTimeInSeconds :: Double
    diffTimeInSeconds = realToFrac diffTime

    hours :: Int
    hours = floor (diffTimeInSeconds / 3600)

    remainingSeconds :: Double
    remainingSeconds = diffTimeInSeconds - fromIntegral (hours * 3600)

    minutes :: Int
    minutes = floor (remainingSeconds / 60)

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
      when (isNothing fleetDriverAssociation) $ throwError (InvalidRequest "Driver is not linked to the fleet")
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
                      ..
                    }
            pure listItem

getListOfDrivers :: Maybe Text -> Maybe Text -> Text -> Id DM.Merchant -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Common.DriverMode -> Flow [FleetDriverAssociation]
getListOfDrivers mbCountryCode mbDriverPhNo fleetOwnerId merchantId mbIsActive mbLimit mbOffset mbMode = do
  case mbDriverPhNo of
    Just driverPhNo -> do
      mobileNumberHash <- getDbHash driverPhNo
      let countryCode = fromMaybe "+91" mbCountryCode
      driver <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchantId DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
      fleetDriverAssociation <- FDV.findByDriverIdAndFleetOwnerId driver.id fleetOwnerId True
      pure $ maybeToList fleetDriverAssociation
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
    createFleetDriverAssociationListItem :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => [FleetDriverAssociation] -> m [Common.DriveVehicleAssociationListItem]
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
        let driverStatus = if isNothing vehicleNo then Nothing else Just $ castDriverStatus driverInfo'.mode
        let isRcAssociated = isJust vehicleNo
        let isDriverActive = fda.isActive
        let driverId = Just $ driver.id.getId
        let ls =
              Common.DriveVehicleAssociationListItem
                { vehicleNo = vehicleNo,
                  status = driverStatus,
                  isDriverActive = isDriverActive,
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
postDriverFleetVehicleDriverRCstatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.RCStatusReq ->
  Flow APISuccess
postDriverFleetVehicleDriverRCstatus merchantShortId opCity reqDriverId fleetOwnerId req = do
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
  let mbSerivceName = DCommon.mapServiceName <$> req.serviceName
  case mbSerivceName of
    Just YATRI_RENTAL -> do
      void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) YATRI_RENTAL (Id <$> req.planToAssociate) req.isActivate req.rcNo
    _ -> pure ()
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
    when (isJust person) $ throwError (InvalidRequest "Mobile number is already linked with another fleet owner")
  when (isJust req.email) $ do
    person <- QPerson.findByEmailAndMerchantIdAndRole req.email merchant.id DP.FLEET_OWNER
    when (isJust person) $ throwError (InvalidRequest "Email is already linked with another fleet owner")
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
  panDetails <- B.runInReplica $ DPC.findByDriverId personId
  panNumber <- case panDetails of
    Just pan -> Just <$> decrypt pan.panCardNumber
    Nothing -> pure Nothing
  makeFleetOwnerInfoRes panNumber fleetOwnerInfo
  where
    makeFleetOwnerInfoRes :: Maybe Text -> DFOI.FleetOwnerInformation -> Flow Common.FleetOwnerInfoRes
    makeFleetOwnerInfoRes panNumber DFOI.FleetOwnerInformation {..} = do
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
    Nothing -> DReg.auth merchantShortId opCity req -------------- to onboard a driver that is not the part of the fleet
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
      fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (InvalidRequest "Fleet Owner not found")
      deviceToken <- fromMaybeM (InvalidRequest "Device Token not found") $ req.deviceToken
      void $ DReg.verify authId True fleetOwnerId Common.AuthVerifyReq {otp = req.otp, deviceToken = deviceToken}
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
      otp <- Redis.get key >>= fromMaybeM (InvalidRequest "OTP not found")
      when (otp /= req.otp) $ throwError (InvalidRequest "Invalid OTP")
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
  unless (rc.verificationStatus == Documents.VALID) $ throwError (InvalidRequest "Cannot link to driver because Rc is not valid")
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
