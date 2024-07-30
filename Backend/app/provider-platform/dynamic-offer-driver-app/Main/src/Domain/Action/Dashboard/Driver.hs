{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver
  ( driverDocumentsInfo,
    driverAadhaarInfo,
    listDrivers,
    driverActivity,
    disableDriver,
    updateACUsageRestriction,
    blockDriverWithReason,
    blockDriver,
    blockReasonList,
    unblockDriver,
    driverLocation,
    deleteDriver,
    unlinkDL,
    unlinkAadhaar,
    updatePhoneNumber,
    updateDriverName,
    clearOnRideStuckDrivers,
    driverAadhaarInfoByPhone,
    updateByPhoneNumber,
    deleteRC,
    getDriverHomeLocation,
    updateDriverHomeLocation,
    incrementDriverGoToCount,
    getDriverGoHomeInfo,
    getPaymentHistoryEntityDetails,
    getPaymentHistory,
    updateSubscriptionDriverFeeAndInvoice,
    sendSmsToDriver,
    SendSmsReq (..),
    VolunteerTransactionStorageReq (..),
    setServiceChargeEligibleFlagInDriverPlan,
    changeOperatingCity,
    getOperatingCity,
    updateRCInvalidStatus,
    updateVehicleVariant,
    bulkReviewRCVariant,
    updateDriverTag,
    castVehicleVariant, -- TODO move to common
    postDriverClearFee,
    mobileIndianCode, -- TODO move to common
    mapServiceName, -- TODO move to common
    castVerificationStatus, -- TODO move to common
    castVehicleVariantDashboard, -- TODO move to common
    notifyYatriRentalEventsToDriver, -- TODO move to common
    runVerifyRCFlow, -- TODO move to common
    appendPlusInMobileCountryCode, -- TODO move to common
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverGoHome as Common
import Control.Applicative ((<|>))
import "dashboard-helper-api" Dashboard.Common (HideSecrets (hideSecrets))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.Dashboard.Management.Merchant as DashboardMerchant
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Action.UI.Driver as Driver
import Domain.Action.UI.DriverGoHomeRequest (CachedGoHomeRequest (..))
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AVD
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DriverBlockReason as DBR
import Domain.Types.DriverFee
import qualified Domain.Types.DriverHomeLocation as DDHL
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverLicense
import Domain.Types.DriverRCAssociation
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Message as Domain
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as SRide
import Domain.Types.TransporterConfig
import qualified Domain.Types.Vehicle as DVeh
import Domain.Types.VehicleRegistrationCertificate
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Kernel.Utils.Validation (runRequestValidation)
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import qualified SharedLogic.DeleteDriver as DeleteDriver
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import qualified SharedLogic.EventTracking as SEVT
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Ride
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQGHC
import Storage.CachedQueries.DriverBlockReason as DBR
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverHomeLocation as QDHL
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Message as MQuery
import qualified Storage.Queries.MessageTranslation as MTQuery
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.RegistrationToken as QR
import Storage.Queries.Ride as QRide
import qualified Storage.Queries.Status as QDocStatus
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.Auth as Auth
import Tools.Error
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

-- FIXME: not tested yet because of no onboarding test data
driverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverDocumentsInfoRes
driverDocumentsInfo merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  let onboardingTryLimit = transporterConfig.onboardingTryLimit
  drivers <- B.runInReplica $ QDocStatus.fetchDriverDocsInfo merchant merchantOpCity Nothing
  pure $ foldl' (func onboardingTryLimit now) Common.emptyInfo drivers
  where
    oneMonth :: NominalDiffTime
    oneMonth = 60 * 60 * 24 * 30
    func :: Int -> UTCTime -> Common.DriverDocumentsInfoRes -> QDocStatus.DriverDocsInfo -> Common.DriverDocumentsInfoRes
    func onboardingTryLimit now acc fd = do
      let mbLic = fd.license
          mbRegCert = fd.assocReg
          mbLicReq = fd.licenseVerificationReq
          mbVehRegReq = fd.regVerificationReq

          dlStatus = getLicenseStatus onboardingTryLimit fd.numLicenseImages mbLic mbLicReq
          rcStatus = getRegCertStatus onboardingTryLimit fd.numVehRegImages mbRegCert mbVehRegReq

          mbDlExpiration = (.licenseExpiry) <$> mbLic
          mbRcExpiration = getRcExpiration . snd <$> mbRegCert
          dlExpiresInMonth = expiresInMonth mbDlExpiration
          rcExpiresInMonth = expiresInMonth mbRcExpiration
          expiresInMonth mbExp = fromMaybe False $
            flip fmap mbExp $
              \exp_ -> let dif = diffUTCTime exp_ now in dif < oneMonth && dif > 0

      acc{Common.registered = acc.registered + 1,
          Common.verified = incrIf fd.driverInfo.verified acc.verified,
          Common.enabled = incrIf fd.driverInfo.enabled acc.enabled,
          Common.blocked = incrIf (not fd.driverInfo.blocked) acc.blocked,
          Common.validDocuments = incrDocs (dlStatus == VALID) (rcStatus == VALID) acc.validDocuments,
          Common.subscribed = incrIf fd.driverInfo.subscribed acc.subscribed,
          Common.invalidDocuments = incrDocs (dlStatus == INVALID) (rcStatus == INVALID) acc.invalidDocuments,
          Common.verificationPending = incrDocs (dlStatus == PENDING) (rcStatus == PENDING) acc.verificationPending,
          Common.verificationFailed = incrDocs (dlStatus == FAILED) (rcStatus == FAILED) acc.verificationFailed,
          Common.verificationLimitExceeded = incrDocs (dlStatus == LIMIT_EXCEED) (rcStatus == LIMIT_EXCEED) acc.verificationLimitExceeded,
          Common.docsExpiringInMonth = incrDocs dlExpiresInMonth rcExpiresInMonth acc.docsExpiringInMonth
         }

incrIf :: Num a => Bool -> a -> a
incrIf b = if b then (+ 1) else identity

incrDocs :: Bool -> Bool -> Common.DocumentsByStateInfo -> Common.DocumentsByStateInfo
incrDocs lic vehReg old =
  old{Common.driverLicense = incrIf lic old.driverLicense,
      Common.vehicleRegistrationCertificate = incrIf vehReg old.vehicleRegistrationCertificate
     }

getRcExpiration :: VehicleRegistrationCertificate -> UTCTime
getRcExpiration = (.fitnessExpiry)

getLicenseStatus :: Int -> Int -> Maybe DriverLicense -> Maybe IV.IdfyVerification -> St.ResponseStatus
getLicenseStatus onboardingTryLimit currentTries mbLicense mbLicReq =
  case mbLicense of
    Just driverLicense -> St.mapStatus driverLicense.verificationStatus
    Nothing -> verificationState onboardingTryLimit currentTries mbLicReq

getRegCertStatus :: Int -> Int -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate) -> Maybe IV.IdfyVerification -> St.ResponseStatus
getRegCertStatus onboardingTryLimit currentTries mbRegCert mbVehRegReq =
  case mbRegCert of
    Just (_assoc, vehicleRC) -> St.mapStatus vehicleRC.verificationStatus
    Nothing -> verificationState onboardingTryLimit currentTries mbVehRegReq

verificationState :: Int -> Int -> Maybe IV.IdfyVerification -> ResponseStatus
verificationState onboardingTryLimit imagesNum verificationReq =
  case verificationReq of
    Just req -> do
      if req.status == "pending"
        then PENDING
        else FAILED
    Nothing -> do
      if imagesNum > onboardingTryLimit
        then LIMIT_EXCEED
        else NO_DOC_AVAILABLE

---------

-- FIXME remove this, all entities should be limited on db level
limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

---------------------------------------------------------------------
listDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
listDrivers merchantShortId opCity mbLimit mbOffset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhone mbVehicleNumberSearchString = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  driversWithInfo <- B.runInReplica $ QPerson.findAllDriversWithInfoAndVehicle merchant merchantOpCity limit offset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString
  items <- mapM buildDriverListItem driversWithInfo
  let count = length items
  -- should we consider filters in totalCount, e.g. count all enabled drivers?
  -- totalCount <- Esq.runInReplica $ QPerson.countDrivers merchant.id
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.DriverListRes {totalItems = count, summary, drivers = items}
  where
    maxLimit = 20
    defaultLimit = 10

buildDriverListItem :: EncFlow m r => (DP.Person, DrInfo.DriverInformation, Maybe DVeh.Vehicle) -> m Common.DriverListItem
buildDriverListItem (person, driverInformation, mbVehicle) = do
  phoneNo <- mapM decrypt person.mobileNumber
  pure $
    Common.DriverListItem
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        vehicleNo = mbVehicle <&> (.registrationNo),
        phoneNo,
        enabled = driverInformation.enabled,
        blocked = driverInformation.blocked,
        verified = driverInformation.verified,
        subscribed = driverInformation.subscribed,
        onRide = driverInformation.onRide,
        active = driverInformation.active,
        onboardingDate = driverInformation.lastEnabledOn
      }

---------------------------------------------------------------------
driverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && driver.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)
  driverInf <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  unless (driverInf.aadhaarVerified) $ throwError $ InvalidRequest "Person aadhaar verification is pending"
  res <- QAadhaarCard.findByPrimaryKey personId
  case res of
    Just aadhaarData -> do
      pure
        Common.DriverAadhaarInfoRes
          { driverName = fromMaybe "Data Not available" aadhaarData.nameOnCard,
            driverGender = fromMaybe "Data Not available" aadhaarData.driverGender,
            driverDob = fromMaybe "Data Not available" aadhaarData.dateOfBirth,
            driverImage = aadhaarData.driverImage
          }
    Nothing -> throwError $ InvalidRequest "no aadhaar data is found"

---------------------------------- -----------------------------------
driverActivity :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverActivityRes
driverActivity merchantShortId _ = do
  merchant <- findMerchantByShortId merchantShortId
  Common.mkDriverActivityRes <$> B.runInReplica (QDriverInfo.countDrivers merchant.id)

---------------------------------------------------------------------
disableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
disableDriver merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  QDriverInfo.updateEnabledVerifiedState driverId False Nothing
  logTagInfo "dashboard -> disableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
updateACUsageRestriction :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateACUsageRestrictionReq -> Flow APISuccess
updateACUsageRestriction merchantShortId opCity reqDriverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
  checkAndUpdateAirConditioned True req.isWorking personId cityVehicleServiceTiers
  logTagInfo "dashboard -> updateACUsageRestriction : " (show personId)
  pure Success

---------------------------------------------------------------------
blockDriverWithReason :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.BlockDriverWithReasonReq -> Flow APISuccess
blockDriverWithReason merchantShortId opCity reqDriverId dashboardUserName req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    B.runInReplica (QPerson.findById personId)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when (driverInf.blocked) $ throwError DriverAccountAlreadyBlocked
  QDriverInfo.updateDynamicBlockedState driverId req.blockReason req.blockTimeInHours dashboardUserName True
  maxShards <- asks (.maxShards)
  case req.blockTimeInHours of
    Just hrs -> do
      let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral hrs) * 60 * 60
      JC.createJobIn @_ @'UnblockDriver unblockDriverJobTs maxShards $
        UnblockDriverRequestJobData
          { driverId = driverId
          }
    Nothing -> return ()
  logTagInfo "dashboard -> blockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
--TODO : To Be Deprecated

blockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
blockDriver merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && driver.merchantOperatingCityId == merchantOpCityId) $ throwError (PersonDoesNotExist personId.getId)
  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when (not driverInf.blocked) (void $ QDriverInfo.updateBlockedState driverId True Nothing)
  logTagInfo "dashboard -> blockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
blockReasonList :: Flow [Common.BlockReason]
blockReasonList = do
  convertToBlockReasonList <$> DBR.findAll

convertToBlockReasonList :: [DBR.DriverBlockReason] -> [Common.BlockReason]
convertToBlockReasonList = map convertToCommon

convertToCommon :: DBR.DriverBlockReason -> Common.BlockReason
convertToCommon res =
  Common.BlockReason
    { reasonCode = cast res.reasonCode,
      blockReason = res.blockReason,
      blockTimeInHours = res.blockTimeInHours
    }

mapServiceName :: Common.ServiceNames -> ServiceNames
mapServiceName common = case common of
  Common.YATRI_SUBSCRIPTION -> YATRI_SUBSCRIPTION
  Common.YATRI_RENTAL -> YATRI_RENTAL

---------------------------------------------------------------------
unblockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
unblockDriver merchantShortId opCity reqDriverId dashboardUserName = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInf.blocked (void $ QDriverInfo.updateBlockedState driverId False (Just dashboardUserName))
  logTagInfo "dashboard -> unblockDriver : " (show personId)
  pure Success

driverLocation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
driverLocation merchantShortId _ mbLimit mbOffset req = do
  merchant <- findMerchantByShortId merchantShortId
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstNameAsc merchant.id driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
      limitedDrivers = limitOffset mbLimit mbOffset allDrivers
  resultList <- mapM buildDriverLocationListItem limitedDrivers
  pure $ Common.DriverLocationRes (nonEmpty $ coerce driversNotFound) resultList

buildDriverLocationListItem :: EncFlow m r => QPerson.FullDriver -> m Common.DriverLocationItem
buildDriverLocationListItem f = do
  let p = f.person
      v = f.vehicle
  phoneNo <- maybe (pure "") decrypt p.mobileNumber
  pure
    Common.DriverLocationItem
      { driverId = cast p.id,
        firstName = p.firstName,
        middleName = p.middleName,
        lastName = p.lastName,
        vehicleNo = v.registrationNo,
        phoneNo,
        active = f.info.active,
        onRide = f.info.onRide,
        location = LatLong f.location.lat f.location.lon,
        lastLocationTimestamp = f.location.coordinatesCalculatedAt
      }

---------------------------------------------------------------------
mobileIndianCode :: Text
mobileIndianCode = "+91"

appendPlusInMobileCountryCode :: Maybe Text -> Maybe Text
appendPlusInMobileCountryCode = fmap (\code -> if "+" `T.isPrefixOf` code then code else "+" <> code)

castVerificationStatus :: Documents.VerificationStatus -> Common.VerificationStatus
castVerificationStatus = \case
  Documents.PENDING -> Common.PENDING
  Documents.VALID -> Common.VALID
  Documents.MANUAL_VERIFICATION_REQUIRED -> Common.MANUAL_VERIFICATION_REQUIRED
  Documents.INVALID -> Common.INVALID
  Documents.UNAUTHORIZED -> Common.UNAUTHORIZED

---------------------------------------------------------------------
deleteDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
deleteDriver merchantShortId _ = DeleteDriver.deleteDriver merchantShortId . cast

---------------------------------------------------------------------
updatePhoneNumber :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
updatePhoneNumber merchantShortId opCity reqDriverId req = do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  phoneNumberHash <- getDbHash req.newPhoneNumber
  mbLinkedPerson <- QPerson.findByMobileNumberAndMerchantAndRole req.newCountryCode phoneNumberHash merchant.id DP.DRIVER
  whenJust mbLinkedPerson $ \linkedPerson -> do
    if linkedPerson.id == driver.id
      then throwError $ InvalidRequest "Person already have the same mobile number"
      else throwError $ InvalidRequest "Person with this mobile number already exists"

  encNewPhoneNumber <- encrypt req.newPhoneNumber
  let updDriver =
        driver
          { DP.mobileCountryCode = Just req.newCountryCode,
            DP.mobileNumber = Just encNewPhoneNumber
          }
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  QPerson.updatePersonDetails updDriver
  QR.deleteByPersonId personId.getId
  logTagInfo "dashboard -> updatePhoneNumber : " (show personId)
  pure Success

---------------------------------------------------------------------
runVerifyRCFlow :: Id DP.Person -> DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.City -> Common.AddVehicleReq -> Bool -> Flow ()
runVerifyRCFlow personId merchant merchantOpCityId operatingCity req isFleet = do
  let vehicleCategory = case req.vehicleCategory of
        Just category -> Just $ DashboardMerchant.castCategory category
        Nothing -> Nothing
  let imageId = maybe "" cast req.imageId
  let rcReq =
        DomainRC.DriverRCReq
          { vehicleRegistrationCertNumber = req.registrationNo,
            imageId = imageId,
            operatingCity = show operatingCity, -- Fixed
            dateOfRegistration = Nothing,
            airConditioned = req.airConditioned,
            oxygen = req.oxygen,
            ventilator = req.ventilator,
            multipleRC = Nothing,
            vehicleDetails = Nothing,
            vehicleCategory = vehicleCategory
          }
  void $ DomainRC.verifyRC (not isFleet) (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq

castVehicleVariant :: Common.Variant -> DVeh.Variant
castVehicleVariant = \case
  Common.SUV -> DVeh.SUV
  Common.HATCHBACK -> DVeh.HATCHBACK
  Common.SEDAN -> DVeh.SEDAN
  Common.AUTO_RICKSHAW -> DVeh.AUTO_RICKSHAW
  Common.TAXI -> DVeh.TAXI
  Common.TAXI_PLUS -> DVeh.TAXI_PLUS
  Common.PREMIUM_SEDAN -> DVeh.PREMIUM_SEDAN
  Common.BLACK -> DVeh.BLACK
  Common.BLACK_XL -> DVeh.BLACK_XL
  Common.BIKE -> DVeh.BIKE
  Common.AMBULANCE_TAXI -> DVeh.AMBULANCE_TAXI
  Common.AMBULANCE_TAXI_OXY -> DVeh.AMBULANCE_TAXI_OXY
  Common.AMBULANCE_AC -> DVeh.AMBULANCE_AC
  Common.AMBULANCE_AC_OXY -> DVeh.AMBULANCE_AC_OXY
  Common.AMBULANCE_VENTILATOR -> DVeh.AMBULANCE_VENTILATOR
  Common.SUV_PLUS -> DVeh.SUV_PLUS

castVehicleVariantDashboard :: Maybe DVeh.Variant -> Maybe Common.Variant
castVehicleVariantDashboard = \case
  Just DVeh.SUV -> Just Common.SUV
  Just DVeh.HATCHBACK -> Just Common.HATCHBACK
  Just DVeh.SEDAN -> Just Common.SEDAN
  Just DVeh.AUTO_RICKSHAW -> Just Common.AUTO_RICKSHAW
  Just DVeh.TAXI -> Just Common.TAXI
  Just DVeh.TAXI_PLUS -> Just Common.TAXI_PLUS
  Just DVeh.PREMIUM_SEDAN -> Just Common.PREMIUM_SEDAN
  Just DVeh.BLACK -> Just Common.BLACK
  Just DVeh.BLACK_XL -> Just Common.BLACK_XL
  Just DVeh.BIKE -> Just Common.BIKE
  Just DVeh.AMBULANCE_TAXI -> Just Common.AMBULANCE_TAXI
  Just DVeh.AMBULANCE_TAXI_OXY -> Just Common.AMBULANCE_TAXI_OXY
  Just DVeh.AMBULANCE_AC -> Just Common.AMBULANCE_AC
  Just DVeh.AMBULANCE_AC_OXY -> Just Common.AMBULANCE_AC_OXY
  Just DVeh.AMBULANCE_VENTILATOR -> Just Common.AMBULANCE_VENTILATOR
  Just DVeh.SUV_PLUS -> Just Common.SUV_PLUS
  _ -> Nothing

---------------------------------------------------------------------
updateDriverName :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
updateDriverName merchantShortId opCity reqDriverId req = do
  runRequestValidation Common.validateUpdateDriverNameReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  -- empty string in request condsidered as Nothing in db, Nothing in request is not affect db value
  let updDriver =
        driver{firstName = req.firstName,
               middleName = if req.middleName == Just "" then Nothing else req.middleName <|> driver.middleName,
               lastName = if req.lastName == Just "" then Nothing else req.lastName <|> driver.lastName
              }

  QPerson.updatePersonRec personId updDriver

  logTagInfo "dashboard -> updateDriverName : " (show personId)
  pure Success

---------------------------------------------------------------------
unlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
unlinkDL merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId_ = cast @Common.Driver @DP.Driver driverId
  let personId = cast @Common.Driver @DP.Person driverId

  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  QDriverLicense.deleteByDriverId personId
  QDriverInfo.updateEnabledVerifiedState driverId_ False (Just False)
  logTagInfo "dashboard -> unlinkDL : " (show personId)
  pure Success

---------------------------------------------------------------------
unlinkAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
unlinkAadhaar merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let driverId_ = cast @Common.Driver @DP.Driver driverId
  let personId = cast @Common.Driver @DP.Person driverId

  _ <- B.runInReplica $ QAadhaarCard.findByPrimaryKey personId >>= fromMaybeM (InvalidRequest "can't unlink Aadhaar")

  QAadhaarCard.deleteByPersonId personId
  QDriverInfo.updateAadhaarVerifiedState False driverId_
  unless (transporterConfig.aadhaarVerificationRequired) $ QDriverInfo.updateEnabledVerifiedState driverId_ False (Just False)
  logTagInfo "dashboard -> unlinkAadhaar : " (show personId)
  pure Success

---------------------------------------------------------------------
deleteRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DeleteRCReq -> Flow APISuccess
deleteRC merchantShortId opCity reqDriverId Common.DeleteRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  DomainRC.deleteRC (personId, merchant.id, merchantOpCityId) (DomainRC.DeleteRCReq {..}) False

getPaymentHistory ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Maybe INV.InvoicePaymentMode ->
  Maybe Int ->
  Maybe Int ->
  ServiceNames ->
  Flow Driver.HistoryEntityV2
getPaymentHistory merchantShortId opCity driverId invoicePaymentMode limit offset serviceName = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getDriverPaymentsHistoryV2 (personId, merchant.id, merchantOpCityId) invoicePaymentMode limit offset serviceName

getPaymentHistoryEntityDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Id INV.Invoice ->
  ServiceNames ->
  Flow Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId invoiceId serviceName = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getHistoryEntryDetailsEntityV2 (personId, merchant.id, merchantOpCityId) invoiceId.getId serviceName

updateSubscriptionDriverFeeAndInvoice ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.ServiceNames ->
  Common.SubscriptionDriverFeesAndInvoicesToUpdate ->
  Flow Common.SubscriptionDriverFeesAndInvoicesToUpdate
updateSubscriptionDriverFeeAndInvoice merchantShortId opCity driverId serviceName' Common.SubscriptionDriverFeesAndInvoicesToUpdate {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  now <- getCurrentTime
  let serviceName = mapServiceName serviceName'
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  maybe (pure ()) (`QDriverInfo.updateSubscription` personId) subscribed
  dueDriverFees <- QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName personId serviceName
  let invoicesDataToUpdate = maybe [] mapToInvoiceInfoToUpdateAfterParse invoices
  mapM_ (\inv -> QINV.updateStatusAndTypeByMbdriverFeeIdAndInvoiceId inv.invoiceId inv.invoiceStatus Nothing inv.driverFeeId) invoicesDataToUpdate
  allDriverFeeByIds <- QDF.findAllByDriverFeeIds (maybe [] (map (\df -> cast (Id df.driverFeeId))) driverFees)
  let reqMkDuesToAmount = (mkDuesToAmountWithCurrency <&> (.amount)) <|> mkDuesToAmount
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  SMerchant.checkCurrencies currency $ do
    let driverFeesFields = flip (maybe []) driverFees $
          concatMap $ \driverFees' ->
            [ driverFees'.platformFeeWithCurrency,
              driverFees'.sgstWithCurrency,
              driverFees'.cgstWithCurrency
            ]
    mkDuesToAmountWithCurrency : driverFeesFields
  if isJust reqMkDuesToAmount
    then do
      let amount = maybe 0.0 (/ (fromIntegral $ length dueDriverFees)) reqMkDuesToAmount
      mapM_ (\fee -> QDF.resetFee fee.id 0 (PlatformFee {fee = amount, cgst = 0.0, sgst = 0.0, currency = fee.currency}) Nothing Nothing now) dueDriverFees
      return $ mkResponse dueDriverFees
    else do
      maybe (pure ()) (updateAccordingToProvidedFeeState currency now) driverFees
      return $ mkResponse allDriverFeeByIds
  where
    mkResponse driverFees' =
      Common.SubscriptionDriverFeesAndInvoicesToUpdate
        { driverFees = Just $ mapToDriverFeeToUpdate driverFees',
          invoices = Nothing,
          mkDuesToAmount = Nothing,
          mkDuesToAmountWithCurrency = Nothing,
          subscribed = Nothing
        }
    mapToInvoiceInfoToUpdateAfterParse =
      map
        ( \invData -> do
            let mbInvoiceStatus = (\invs -> readMaybe (T.unpack invs) :: (Maybe INV.InvoiceStatus)) =<< invData.invoiceStatus
            InvoiceInfoToUpdateAfterParse
              { invoiceId = cast (Id invData.invoiceId),
                driverFeeId = cast . Id <$> invData.driverFeeId,
                invoiceStatus = mbInvoiceStatus
              }
        )
    mapToDriverFeeToUpdate =
      map
        ( \dfee ->
            Common.DriverFeeInfoToUpdate
              { driverFeeId = dfee.id.getId,
                mkManualDue = Nothing,
                mkAutoPayDue = Nothing,
                mkCleared = Nothing,
                platformFee = Just $ dfee.platformFee.fee,
                cgst = Just dfee.platformFee.cgst,
                sgst = Just dfee.platformFee.sgst,
                platformFeeWithCurrency = Just $ PriceAPIEntity dfee.platformFee.fee dfee.currency,
                cgstWithCurrency = Just $ PriceAPIEntity dfee.platformFee.cgst dfee.currency,
                sgstWithCurrency = Just $ PriceAPIEntity dfee.platformFee.sgst dfee.currency
              }
        )
    updateAccordingToProvidedFeeState currency now =
      mapM_
        ( \fee -> do
            let id = cast (Id fee.driverFeeId)
                platFormFee' = fromMaybe 0 ((fee.platformFeeWithCurrency <&> (.amount)) <|> fee.platformFee)
                sgst = fromMaybe 0 ((fee.sgstWithCurrency <&> (.amount)) <|> fee.sgst)
                cgst = fromMaybe 0 ((fee.cgstWithCurrency <&> (.amount)) <|> fee.cgst)
                platFormFee = PlatformFee {fee = platFormFee', sgst, cgst, currency}
            QDF.resetFee id 0 platFormFee Nothing Nothing now
            when (fee.mkManualDue == Just True) $ do QDF.updateAutoPayToManual id
            when (fee.mkAutoPayDue == Just True && fee.mkManualDue `elem` [Nothing, Just False]) $ do QDF.updateManualToAutoPay id
        )

data InvoiceInfoToUpdateAfterParse = InvoiceInfoToUpdateAfterParse
  { invoiceId :: Id INV.Invoice,
    driverFeeId :: Maybe (Id DriverFee),
    invoiceStatus :: Maybe INV.InvoiceStatus
  }

---------------------------------------------------------------------
clearOnRideStuckDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Flow Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId _ dbSyncTime = do
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  let dbSyncInterVal = addUTCTime (fromIntegral (- fromMaybe 1 dbSyncTime) * 60) now
  driverInfosAndRideDetails <- B.runInReplica $ QPerson.getOnRideStuckDriverIds dbSyncInterVal
  driverIds <-
    mapM
      ( \dI -> do
          updateOnRideStatusWithAdvancedRideCheck (cast dI.driverInfo.driverId) (Just dI.ride)
          void $ LF.rideDetails dI.ride.id SRide.CANCELLED merchant.id dI.ride.driverId dI.ride.fromLocation.lat dI.ride.fromLocation.lon
          return (cast dI.driverInfo.driverId)
      )
      driverInfosAndRideDetails

  return Common.ClearOnRideStuckDriversRes {driverIds = driverIds}

---------------------------------------------------------------------
getDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.GetHomeLocationsRes
getDriverHomeLocation merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  dghLocs <- DDriver.getHomeLocations (cast driverId, cast merchant.id, merchantOpCityId)
  return (buildDriverHomeLocationAPIEntity <$> dghLocs.locations)
  where
    buildDriverHomeLocationAPIEntity dghLocs =
      Common.DriverHomeLocationAPIEntity
        { id = cast dghLocs.id,
          address = dghLocs.address,
          lat = dghLocs.lat,
          lon = dghLocs.lon,
          tag = dghLocs.tag
        }

updateDriverHomeLocation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverHomeLocationReq -> Flow APISuccess
updateDriverHomeLocation _ _ _ req = do
  QDHL.updateHomeLocationById (cast req.id) buildDriverHomeLocationEntity
  return Success
  where
    buildDriverHomeLocationEntity =
      DDHL.UpdateDriverHomeLocation
        { address = req.address,
          lat = req.lat,
          lon = req.lon,
          tag = req.tag
        }

incrementDriverGoToCount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
incrementDriverGoToCount merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  CQDGR.increaseDriverGoHomeRequestCount merchantOpCityId (cast driverId)
  return Success

getDriverGoHomeInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.CachedGoHomeRequestInfoRes
getDriverGoHomeInfo merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  ghInfo <- CQGHC.getDriverGoHomeRequestInfo (cast driverId) merchantOpCityId Nothing
  return (buildCachedGoHomeRequestInfoRes ghInfo)
  where
    buildCachedGoHomeRequestInfoRes CachedGoHomeRequest {..} =
      Common.CachedGoHomeRequestInfoRes
        { status = show <$> status,
          driverGoHomeRequestId = cast <$> driverGoHomeRequestId,
          ..
        }

---------------------------------------------------------------------
driverAadhaarInfoByPhone :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId _ phoneNumber = do
  merchant <- findMerchantByShortId merchantShortId
  mobileNumberHash <- getDbHash phoneNumber
  driver <- QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
  res <- QAadhaarCard.findByPrimaryKey driver.id
  case res of
    Just aadhaarData -> do
      pure
        Common.DriverAadhaarInfoRes
          { driverName = fromMaybe "Data Not Available" aadhaarData.nameOnCard,
            driverGender = fromMaybe "Data Not Available" aadhaarData.driverGender,
            driverDob = fromMaybe "Data Not Available" aadhaarData.dateOfBirth,
            driverImage = aadhaarData.driverImage
          }
    Nothing -> throwError $ InvalidRequest "no aadhaar data is found"

---------------------------------------------------------------------
updateByPhoneNumber :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateDriverDataReq -> Flow APISuccess
updateByPhoneNumber merchantShortId _ phoneNumber req = do
  mobileNumberHash <- getDbHash phoneNumber
  aadhaarNumberHash <- getDbHash req.driverAadhaarNumber
  aadhaarInfo <- QAadhaarCard.findByAadhaarNumberHash (Just aadhaarNumberHash)
  when (isJust aadhaarInfo) $ throwError AadhaarAlreadyLinked
  merchant <- findMerchantByShortId merchantShortId
  driver <- QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
  res <- QAadhaarCard.findByPrimaryKey driver.id
  case res of
    Just _ -> QAadhaarCard.findByPhoneNumberAndUpdate (Just req.driverName) (Just req.driverGender) (Just req.driverDob) (Just aadhaarNumberHash) (bool Documents.INVALID Documents.VALID req.isVerified) driver.id
    Nothing -> do
      aadhaarEntity <- AVD.mkAadhaar merchant.id driver.merchantOperatingCityId driver.id req.driverName req.driverGender req.driverDob (Just aadhaarNumberHash) Nothing True Nothing
      QAadhaarCard.create aadhaarEntity
  QDriverInfo.updateAadhaarVerifiedState True (cast driver.id)
  pure Success

------------------------------------------------------------------------------------------------
data SendSmsReq = SendSmsReq
  { channel :: MediaChannel,
    messageKey :: Maybe MessageKey,
    overlayKey :: Maybe Text,
    messageId :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance HideSecrets SendSmsReq where
  hideSecrets = identity

data VolunteerTransactionStorageReq = VolunteerTransactionStorageReq
  { volunteerId :: Text,
    driverId :: Text,
    messageKey :: Text,
    channel :: Text,
    overlayKey :: Text,
    messageId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance HideSecrets VolunteerTransactionStorageReq where
  hideSecrets = identity

sendSmsToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> SendSmsReq -> Flow APISuccess
sendSmsToDriver merchantShortId opCity driverId volunteerId _req@SendSmsReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- limit checking
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  void $ checkIfVolunteerSMSSendingLimitExceeded volunteerId transporterConfig.volunteerSmsSendingLimit channel
  void $ checkIfDriverSMSReceivingLimitExceeded driverId.getId transporterConfig.driverSmsReceivingLimit channel

  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access check
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  smsCfg <- asks (.smsCfg)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
  withLogTag ("personId_" <> personId.getId) $ do
    case channel of
      SMS -> do
        mkey <- fromMaybeM (InvalidRequest "Message Key field is required for channel : SMS") messageKey --whenJust messageKey $ \mkey -> do
        (mbSender, message) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        mkey <- fromMaybeM (InvalidRequest "Message Key field is required for channel : WHATSAPP") messageKey -- whenJust messageKey $ \mkey -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKey merchantOpCityId mkey
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        let jsonData = merchantMessage.jsonData
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId jsonData.var1 jsonData.var2 jsonData.var3 Nothing (Just merchantMessage.containsUrlButton))
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      OVERLAY -> do
        oKey <- fromMaybeM (InvalidRequest "Overlay Key field is required for channel : OVERLAY") overlayKey --whenJust overlayKey $ \oKey -> do
        manualDues <- getManualDues personId transporterConfig.timeDiffFromUtc transporterConfig.driverFeeOverlaySendingTimeLimitInDays
        overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf merchantOpCityId oKey (fromMaybe ENGLISH driver.language) Nothing >>= fromMaybeM (OverlayKeyNotFound oKey)
        let okButtonText = T.replace (templateText "dueAmount") (show manualDues) <$> overlay.okButtonText
        let description = T.replace (templateText "dueAmount") (show manualDues) <$> overlay.description
        let overlay' = overlay{okButtonText, description}
        TN.sendOverlay merchantOpCityId driver $ TN.mkOverlayReq overlay'
      ALERT -> do
        _mId <- fromMaybeM (InvalidRequest "Message Id field is required for channel : ALERT") messageId -- whenJust messageId $ \_mId -> do
        topicName <- asks (.broadcastMessageTopic)
        message <- B.runInReplica $ MQuery.findById (Id _mId) >>= fromMaybeM (InvalidRequest "Message Not Found")
        msg <- createMessageLanguageDict message
        produceMessage (topicName, Just (encodeUtf8 $ getId driverId)) msg
  -- if the message is sent successfuly then increment the count of both volunteer and driver
  void $ incrementVolunteerSMSSendingCount volunteerId channel
  void $ incrementDriverSMSReceivingCount driverId.getId channel
  pure Success
  where
    createMessageLanguageDict :: Domain.RawMessage -> Flow Domain.MessageDict
    createMessageLanguageDict message = do
      translations <- B.runInReplica $ MTQuery.findByMessageId message.id
      pure $ Domain.MessageDict message (M.fromList $ map (addTranslation message) translations)

    addTranslation Domain.RawMessage {..} trans =
      (show trans.language, Domain.RawMessage {title = trans.title, description = trans.description, shortDescription = trans.shortDescription, label = trans.label, ..})

    getManualDues personId timeDiffFromUtc driverFeeOverlaySendingTimeLimitInDays = do
      windowEndTime <- getLocalCurrentTime timeDiffFromUtc
      let windowStartTime = addUTCTime (-1 * fromIntegral driverFeeOverlaySendingTimeLimitInDays * 86400) (UTCTime (utctDay windowEndTime) (secondsToDiffTime 0))
      pendingDriverFees <- QDF.findAllOverdueDriverFeeByDriverIdForServiceName personId YATRI_SUBSCRIPTION
      let filteredDriverFees = filter (\driverFee -> driverFee.startTime >= windowStartTime) pendingDriverFees
      return $
        if null filteredDriverFees
          then 0
          else sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

    templateText txt = "{#" <> txt <> "#}"

changeOperatingCity :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.ChangeOperatingCityReq -> Flow APISuccess
changeOperatingCity merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  merchantOpCityId' <- CQMOC.getMerchantOpCityId Nothing merchant (Just $ req.operatingCity)
  QPerson.updateMerchantOperatingCityId personId merchantOpCityId'
  QReg.updateMerchantOperatingCityId merchantOpCityId'.getId personId.getId merchant.id.getId
  DReg.cleanCachedTokens personId
  pure Success

windowLimit :: SWC.SlidingWindowOptions
windowLimit = SWC.SlidingWindowOptions 24 SWC.Hours

checkIfVolunteerSMSSendingLimitExceeded :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DashboardMediaSendingLimit -> MediaChannel -> m ()
checkIfVolunteerSMSSendingLimitExceeded volunteerId limitConfig channel = do
  let limit = case limitConfig of
        Nothing -> 500
        Just config -> getLimitAccordingToChannel config channel
  (currentLimit :: Int) <- fromIntegral <$> SWC.getCurrentWindowCount (mkVolunteerSMSSendingLimitKey volunteerId channel) windowLimit
  when (currentLimit >= limit) $ throwError (VolunteerMessageSendingLimitExceeded (show channel)) -- the limit is counted from 0

incrementVolunteerSMSSendingCount :: (CacheFlow m r, MonadFlow m) => Text -> MediaChannel -> m ()
incrementVolunteerSMSSendingCount volunteerId channel = SWC.incrementWindowCount (mkVolunteerSMSSendingLimitKey volunteerId channel) windowLimit

checkIfDriverSMSReceivingLimitExceeded :: (CacheFlow m r, MonadFlow m) => Text -> Maybe DashboardMediaSendingLimit -> MediaChannel -> m ()
checkIfDriverSMSReceivingLimitExceeded driverId limitConfig channel = do
  let limit = case limitConfig of
        Nothing -> 10
        Just config -> getLimitAccordingToChannel config channel
  (currentLimit :: Int) <- fromIntegral <$> SWC.getCurrentWindowCount (mkDriverSMSRecevingLimitKey driverId channel) windowLimit
  when (currentLimit >= limit) $ throwError (DriverMessageReceivingLimitExceeded (show channel)) -- the limit is counted from 0

incrementDriverSMSReceivingCount :: (CacheFlow m r, MonadFlow m) => Text -> MediaChannel -> m ()
incrementDriverSMSReceivingCount driverId channel = SWC.incrementWindowCount (mkDriverSMSRecevingLimitKey driverId channel) windowLimit

mkVolunteerSMSSendingLimitKey :: Text -> MediaChannel -> Text
mkVolunteerSMSSendingLimitKey volunteerId channel = "Dashboard:VolunteerId-" <> volunteerId <> ":channel-" <> show channel <> ":SendSMS:HitCount"

mkDriverSMSRecevingLimitKey :: Text -> MediaChannel -> Text
mkDriverSMSRecevingLimitKey driverId channel = "Dashboard:DriverId-" <> driverId <> ":channel-" <> show channel <> ":SendSMS:ReceiveCount"

getLimitAccordingToChannel :: DashboardMediaSendingLimit -> MediaChannel -> Int
getLimitAccordingToChannel config channel =
  case channel of
    SMS -> config.sms
    WHATSAPP -> config.whatsapp
    OVERLAY -> config.overlay
    ALERT -> config.alert

--------------------------------------------------------------------------------------------------
getOperatingCity :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe (Id Common.Ride) -> Flow Common.GetOperatingCityResp
getOperatingCity merchantShortId _ mbMobileCountryCode mbMobileNumber mbRideId = do
  merchant <- findMerchantByShortId merchantShortId
  case (mbRideId, mbMobileNumber) of
    (Just rideId, _) -> do
      let rId = cast @Common.Ride @SRide.Ride rideId
      ride <- QRide.findById rId >>= fromMaybeM (RideNotFound rId.getId)
      city <- CQMOC.findById ride.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId)
      return $ Common.GetOperatingCityResp {operatingCity = city.city}
    (_, Just mobileNumber) -> do
      mobileNumberHash <- getDbHash mobileNumber
      driver <- QPerson.findByMobileNumberAndMerchantAndRole (fromMaybe "+91" (appendPlusInMobileCountryCode mbMobileCountryCode)) mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
      let operatingCityId = driver.merchantOperatingCityId
      city <- CQMOC.findById operatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId)
      return $ Common.GetOperatingCityResp {operatingCity = city.city}
    _ -> throwError $ InvalidRequest "Either rideId or mobileNumber is required"

setServiceChargeEligibleFlagInDriverPlan :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.PauseOrResumeServiceChargesReq -> Flow APISuccess
setServiceChargeEligibleFlagInDriverPlan merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = mapServiceName req.serviceName
  driverPlan <- QDP.findByDriverIdWithServiceName personId serviceName
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let mbEnableServiceUsageCharge = driverPlan <&> (.enableServiceUsageCharge)
  when (mbEnableServiceUsageCharge /= Just req.serviceChargeEligibility) $ do
    QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName req.serviceChargeEligibility personId serviceName
    fork "track service toggle" $ do
      case driverPlan of
        Just dp -> SEVT.trackServiceUsageChargeToggle dp (show <$> req.reason)
        Nothing -> pure ()
    when (serviceName == YATRI_RENTAL) $ do
      fork "notify rental event" $ do
        notifyYatriRentalEventsToDriver req.vehicleId (getMkeyForEvent req.serviceChargeEligibility) personId transporterConfig (show <$> req.reason) WHATSAPP
  pure Success
  where
    getMkeyForEvent serviceChargeEligiblity = if serviceChargeEligiblity then YATRI_RENTAL_RESUME else YATRI_RENTAL_PAUSE

notifyYatriRentalEventsToDriver :: Text -> MessageKey -> Id DP.Person -> TransporterConfig -> Maybe Text -> MediaChannel -> Flow ()
notifyYatriRentalEventsToDriver vehicleId messageKey personId transporterConfig mbReason channel = do
  smsCfg <- asks (.smsCfg)
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  nowLocale <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let phoneNumber = countryCode <> mobileNumber
      timeStamp = show $ utctDay nowLocale
      merchantOpCityId = transporterConfig.merchantOperatingCityId
      mkey = messageKey
  withLogTag ("personId_" <> personId.getId) $ do
    case channel of
      SMS -> do
        (mbSender, message) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKey merchantOpCityId mkey
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId (Just vehicleId) (Just timeStamp) mbReason Nothing (Just merchantMessage.containsUrlButton))
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      _ -> pure ()

updateRCInvalidStatus :: ShortId DM.Merchant -> Context.City -> Common.UpdateRCInvalidStatusReq -> Flow APISuccess
updateRCInvalidStatus _ _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  let variant = castVehicleVariant req.vehicleVariant
  RCQuery.updateVehicleVariant vehicleRC.id (Just variant) Nothing (Just True)
  pure Success

updateVehicleVariant :: ShortId DM.Merchant -> Context.City -> Common.UpdateVehicleVariantReq -> Flow APISuccess
updateVehicleVariant _ _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  rcNumber <- decrypt vehicleRC.certificateNumber
  let variant = castVehicleVariant req.vehicleVariant
  mVehicle <- QVehicle.findByRegistrationNo rcNumber
  RCQuery.updateVehicleVariant vehicleRC.id (Just variant) Nothing Nothing
  whenJust mVehicle $ \vehicle -> updateVehicleVariantAndServiceTier variant vehicle
  pure Success

bulkReviewRCVariant :: ShortId DM.Merchant -> Context.City -> [Common.ReviewRCVariantReq] -> Flow [Common.ReviewRCVariantRes]
bulkReviewRCVariant _ _ req = do
  mapM
    ( \rcReq -> do
        res <- try @_ @SomeException (processRCReq rcReq)
        case res of
          Left err -> pure $ Common.ReviewRCVariantRes rcReq.rcId (show err)
          Right _ -> pure $ Common.ReviewRCVariantRes rcReq.rcId "Success"
    )
    req
  where
    processRCReq rcReq = do
      vehicleRC <- RCQuery.findById (Id rcReq.rcId) >>= fromMaybeM (VehicleNotFound rcReq.rcId)
      let mbVariant = castVehicleVariant <$> rcReq.vehicleVariant
      rcNumber <- decrypt vehicleRC.certificateNumber
      mVehicle <- QVehicle.findByRegistrationNo rcNumber
      RCQuery.updateVehicleVariant vehicleRC.id mbVariant rcReq.markReviewed (not <$> rcReq.markReviewed)
      whenJust mVehicle $ \vehicle -> do
        whenJust mbVariant $ \variant -> updateVehicleVariantAndServiceTier variant vehicle

updateVehicleVariantAndServiceTier :: DVeh.Variant -> DVeh.Vehicle -> Flow ()
updateVehicleVariantAndServiceTier variant vehicle = do
  driver <- B.runInReplica $ QPerson.findById vehicle.driverId >>= fromMaybeM (PersonDoesNotExist vehicle.driverId.getId)
  driverInfo' <- QDriverInfo.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  -- driverStats <- runInReplica $ QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId driver.merchantOperatingCityId
  let availableServiceTiersForDriver = (.serviceTierType) . fst <$> selectVehicleTierForDriverWithUsageRestriction True driverInfo' vehicle vehicleServiceTiers
  QVehicle.updateVariantAndServiceTiers variant availableServiceTiersForDriver vehicle.driverId

updateDriverTag :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverTagReq -> Flow APISuccess
updateDriverTag merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let tag =
        if req.isAddingTag
          then addDriverTag driver.driverTag (T.toUpper req.driverTag)
          else removeDriverTag driver.driverTag (T.toUpper req.driverTag)
  QPerson.updateTag personId tag
  pure Success

addDriverTag :: Maybe [Text] -> Text -> [Text]
addDriverTag Nothing tag = [tag]
addDriverTag (Just tags) tag = tags ++ [tag]

removeDriverTag :: Maybe [Text] -> Text -> [Text]
removeDriverTag Nothing _ = []
removeDriverTag (Just tags) tag = filter (/= tag) tags

postDriverClearFee :: (ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.ClearDriverFeeReq -> Flow APISuccess)
postDriverClearFee _merchantShortId _opCity driverId req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = mapServiceName req.serviceName
  let feeType = castCommonFeeTypeToDomainFeeType req.feeType
  let currency = fromMaybe INR req.currency
      gstPercentages = (,) <$> req.sgstPercentage <*> req.cgstPercentage
  void $ DDriver.clearDriverFeeWithCreate (personId, driver.merchantId, merchantOpCityId) serviceName (gstBreakup gstPercentages req.platformFee) feeType currency Nothing req.sendManualLink
  return Kernel.Types.APISuccess.Success
  where
    castCommonFeeTypeToDomainFeeType feeTypeCommon = case feeTypeCommon of
      Common.PAYOUT_REGISTRATION -> PAYOUT_REGISTRATION
      Common.ONE_TIME_SECURITY_DEPOSIT -> ONE_TIME_SECURITY_DEPOSIT
    gstBreakup gstPercentages fee = case gstPercentages of
      Just (sgstPer, cgstPer) -> (fee * (1.0 - ((cgstPer + sgstPer) / 100.0)), Just $ (cgstPer * fee) / 100.0, Just $ (sgstPer * fee) / 100.0)
      _ -> (fee, Nothing, Nothing)
