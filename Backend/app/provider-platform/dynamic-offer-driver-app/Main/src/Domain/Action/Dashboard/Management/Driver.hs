{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.Driver
  ( getDriverDocumentsInfo,
    postDriverPersonNumbers,
    getDriverAadhaarInfo,
    getDriverAadhaarInfobyMobileNumber,
    getDriverList,
    getDriverActivity,
    postDriverDisable,
    postDriverAcRestrictionUpdate,
    postDriverBlockWithReason,
    postDriverBlock,
    getDriverBlockReasonList,
    postDriverUnblock,
    getDriverLocation,
    deleteDriverPermanentlyDelete,
    postDriverUnlinkDL,
    postDriverUnlinkAadhaar,
    postDriverUpdatePhoneNumber,
    postDriverUpdateByPhoneNumber,
    postDriverUpdateName,
    postDriverDeleteRC,
    getDriverClearStuckOnRide,
    postDriverSendDummyNotification,
    postDriverChangeOperatingCity,
    getDriverGetOperatingCity,
    postDriverPauseOrResumeServiceCharges,
    postDriverUpdateRCInvalidStatus,
    postDriverUpdateVehicleVariant,
    postDriverBulkReviewRCVariant,
    postDriverUpdateDriverTag,
    postDriverClearFee,
    getDriverPanAadharSelfieDetails,
    postDriverSyncDocAadharPan,
  )
where

import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.Driver as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Csv
import Data.List (sortOn)
import Data.List.NonEmpty (nonEmpty)
import Data.List.Split (chunksOf)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Vector as V
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Management.Driver.Notification as DDN
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AVD
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DocumentVerificationConfig as DomainDVC
import qualified Domain.Types.DriverBlockReason as DBR
import Domain.Types.DriverFee
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverLicense
import Domain.Types.DriverRCAssociation
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.Vehicle as DVeh
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified Lib.Yudhishthira.Flow.Dashboard as Yudhishthira
import SharedLogic.Allocator
import qualified SharedLogic.DeleteDriver as DeleteDriver
import SharedLogic.DriverOnboarding
import qualified SharedLogic.EventTracking as SEVT
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.Ride
import SharedLogic.VehicleServiceTier
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC
import Storage.CachedQueries.DriverBlockReason as DBR
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.RegistrationToken as QR
import Storage.Queries.Ride as QRide
import qualified Storage.Queries.Status as QDocStatus
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.Auth as Auth
import Tools.Error

-- FIXME: not tested yet because of no onboarding test data
getDriverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverDocumentsInfoRes
getDriverDocumentsInfo merchantShortId opCity = do
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

---------------------------------------------------------------------
postDriverPersonNumbers :: ShortId DM.Merchant -> Context.City -> Common.PersonIdsReq -> Flow [Common.PersonRes]
postDriverPersonNumbers _ _ req = do
  csvData <- readCsvAndGetPersonIds req.file
  let chunks = chunksOf 100 csvData
  decryptedNumbers <- forM chunks processChunk
  return $ concat decryptedNumbers
  where
    readCsvAndGetPersonIds :: FilePath -> Flow [Text]
    readCsvAndGetPersonIds csvFile = do
      csvData <- liftIO $ BS.readFile csvFile
      case decodeByName (LBS.fromStrict csvData) :: Either String (V.Vector BS.ByteString, V.Vector Common.PersonIdsCsvRow) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> pure $ map (.personId) $ V.toList v

    processChunk :: [Text] -> Flow [Common.PersonRes]
    processChunk chunk = do
      persons <- QPerson.findAllByPersonIds chunk
      decryptedPersons <- forM persons $ \p -> do
        decPerson <- decrypt p
        return $ Common.PersonRes decPerson.id.getId decPerson.mobileNumber decPerson.alternateMobileNumber decPerson.merchantOperatingCityId.getId
      return decryptedPersons

---------------------------------------------------------------------
getDriverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DriverAadhaarInfoRes
getDriverAadhaarInfo merchantShortId opCity driverId = do
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

---------------------------------------------------------------------
getDriverAadhaarInfobyMobileNumber :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.DriverAadhaarInfoByPhoneReq
getDriverAadhaarInfobyMobileNumber merchantShortId _ phoneNumber = do
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
getDriverList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
getDriverList merchantShortId opCity mbLimit mbOffset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhone mbVehicleNumberSearchString = do
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
getDriverActivity :: ShortId DM.Merchant -> Context.City -> Flow Common.DriverActivityRes
getDriverActivity merchantShortId _ = do
  merchant <- findMerchantByShortId merchantShortId
  Common.mkDriverActivityRes <$> B.runInReplica (QDriverInfo.countDrivers merchant.id)

---------------------------------------------------------------------
postDriverDisable :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverDisable merchantShortId opCity reqDriverId = do
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
postDriverAcRestrictionUpdate :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateACUsageRestrictionReq -> Flow APISuccess
postDriverAcRestrictionUpdate merchantShortId opCity reqDriverId req = do
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
postDriverBlockWithReason :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.BlockDriverWithReasonReq -> Flow APISuccess
postDriverBlockWithReason merchantShortId opCity reqDriverId dashboardUserName req = do
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
postDriverBlock :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverBlock merchantShortId opCity reqDriverId = do
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
getDriverBlockReasonList :: ShortId DM.Merchant -> Context.City -> Flow [Common.BlockReason]
getDriverBlockReasonList _ _ = do
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

---------------------------------------------------------------------
postDriverUnblock :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
postDriverUnblock merchantShortId opCity reqDriverId dashboardUserName = do
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

---------------------------------------------------------------------
getDriverLocation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
getDriverLocation merchantShortId _ mbLimit mbOffset req = do
  merchant <- findMerchantByShortId merchantShortId
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstNameAsc merchant.id driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
      limitedDrivers = limitOffset mbLimit mbOffset allDrivers
  resultList <- mapM buildDriverLocationListItem limitedDrivers
  pure $ Common.DriverLocationRes (nonEmpty $ coerce driversNotFound) resultList

-- FIXME remove this, all entities should be limited on db level
limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

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
deleteDriverPermanentlyDelete :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
deleteDriverPermanentlyDelete merchantShortId _ = DeleteDriver.deleteDriver merchantShortId . cast

---------------------------------------------------------------------
postDriverUnlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkDL merchantShortId opCity driverId = do
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
postDriverUnlinkAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkAadhaar merchantShortId opCity driverId = do
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
postDriverUpdatePhoneNumber :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
postDriverUpdatePhoneNumber merchantShortId opCity reqDriverId req = do
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
postDriverUpdateByPhoneNumber :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateDriverDataReq -> Flow APISuccess
postDriverUpdateByPhoneNumber merchantShortId _ phoneNumber req = do
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

---------------------------------------------------------------------
postDriverUpdateName :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
postDriverUpdateName merchantShortId opCity reqDriverId req = do
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
postDriverDeleteRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DeleteRCReq -> Flow APISuccess
postDriverDeleteRC merchantShortId opCity reqDriverId Common.DeleteRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  DomainRC.deleteRC (personId, merchant.id, merchantOpCityId) (DomainRC.DeleteRCReq {..}) False

---------------------------------------------------------------------
getDriverClearStuckOnRide :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Flow Common.ClearOnRideStuckDriversRes
getDriverClearStuckOnRide merchantShortId _ dbSyncTime = do
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
postDriverSendDummyNotification :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverSendDummyNotification = DDN.sendDummyRideRequestToDriver

---------------------------------------------------------------------
postDriverChangeOperatingCity :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.ChangeOperatingCityReq -> Flow APISuccess
postDriverChangeOperatingCity merchantShortId opCity driverId req = do
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

---------------------------------------------------------------------
getDriverGetOperatingCity :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe (Id Common.Ride) -> Flow Common.GetOperatingCityResp
getDriverGetOperatingCity merchantShortId _ mbMobileCountryCode mbMobileNumber mbRideId = do
  merchant <- findMerchantByShortId merchantShortId
  case (mbRideId, mbMobileNumber) of
    (Just rideId, _) -> do
      let rId = cast @Common.Ride @SRide.Ride rideId
      ride <- QRide.findById rId >>= fromMaybeM (RideNotFound rId.getId)
      city <- CQMOC.findById ride.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId)
      return $ Common.GetOperatingCityResp {operatingCity = city.city}
    (_, Just mobileNumber) -> do
      mobileNumberHash <- getDbHash mobileNumber
      driver <- QPerson.findByMobileNumberAndMerchantAndRole (fromMaybe "+91" (DCommon.appendPlusInMobileCountryCode mbMobileCountryCode)) mobileNumberHash merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
      let operatingCityId = driver.merchantOperatingCityId
      city <- CQMOC.findById operatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId)
      return $ Common.GetOperatingCityResp {operatingCity = city.city}
    _ -> throwError $ InvalidRequest "Either rideId or mobileNumber is required"

---------------------------------------------------------------------
-- setServiceChargeEligibleFlagInDriverPlan
postDriverPauseOrResumeServiceCharges :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.PauseOrResumeServiceChargesReq -> Flow APISuccess
postDriverPauseOrResumeServiceCharges merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = DCommon.mapServiceName req.serviceName
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
        DCommon.notifyYatriRentalEventsToDriver req.vehicleId (getMkeyForEvent req.serviceChargeEligibility) personId transporterConfig (show <$> req.reason) WHATSAPP
  pure Success
  where
    getMkeyForEvent serviceChargeEligiblity = if serviceChargeEligiblity then YATRI_RENTAL_RESUME else YATRI_RENTAL_PAUSE

---------------------------------------------------------------------
postDriverUpdateRCInvalidStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateRCInvalidStatusReq -> Flow APISuccess
postDriverUpdateRCInvalidStatus _merchantShortId _opCity _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  RCQuery.updateVehicleVariant vehicleRC.id (Just req.vehicleVariant) Nothing (Just True)
  pure Success

---------------------------------------------------------------------
postDriverUpdateVehicleVariant :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateVehicleVariantReq -> Flow APISuccess
postDriverUpdateVehicleVariant _ _ _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  rcNumber <- decrypt vehicleRC.certificateNumber
  mVehicle <- QVehicle.findByRegistrationNo rcNumber
  RCQuery.updateVehicleVariant vehicleRC.id (Just req.vehicleVariant) Nothing Nothing
  whenJust mVehicle $ \vehicle -> updateVehicleVariantAndServiceTier req.vehicleVariant vehicle
  pure Success

updateVehicleVariantAndServiceTier :: DV.VehicleVariant -> DVeh.Vehicle -> Flow ()
updateVehicleVariantAndServiceTier variant vehicle = do
  driver <- B.runInReplica $ QPerson.findById vehicle.driverId >>= fromMaybeM (PersonDoesNotExist vehicle.driverId.getId)
  driverInfo' <- QDriverInfo.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  -- driverStats <- runInReplica $ QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId driver.merchantOperatingCityId
  let availableServiceTiersForDriver = (.serviceTierType) . fst <$> selectVehicleTierForDriverWithUsageRestriction True driverInfo' vehicle vehicleServiceTiers
  QVehicle.updateVariantAndServiceTiers variant availableServiceTiersForDriver vehicle.driverId

---------------------------------------------------------------------
postDriverBulkReviewRCVariant :: ShortId DM.Merchant -> Context.City -> [Common.ReviewRCVariantReq] -> Flow [Common.ReviewRCVariantRes]
postDriverBulkReviewRCVariant _ _ req = do
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
      rcNumber <- decrypt vehicleRC.certificateNumber
      mVehicle <- QVehicle.findByRegistrationNo rcNumber
      RCQuery.updateVehicleVariant vehicleRC.id rcReq.vehicleVariant rcReq.markReviewed (not <$> rcReq.markReviewed)
      whenJust mVehicle $ \vehicle -> do
        whenJust rcReq.vehicleVariant $ \variant -> updateVehicleVariantAndServiceTier variant vehicle

---------------------------------------------------------------------
postDriverUpdateDriverTag :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverTagReq -> Flow APISuccess
postDriverUpdateDriverTag merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Yudhishthira.verifyTag req.driverTag ---------------------------
  let tag =
        if req.isAddingTag
          then addDriverTag driver.driverTag req.driverTag
          else removeDriverTag driver.driverTag req.driverTag
  QPerson.updateTag personId tag
  pure Success

addDriverTag :: Maybe [Text] -> Text -> [Text]
addDriverTag Nothing tag = [tag]
addDriverTag (Just tags) tag = tags ++ [tag]

removeDriverTag :: Maybe [Text] -> Text -> [Text]
removeDriverTag Nothing _ = []
removeDriverTag (Just tags) tag = filter (/= tag) tags

---------------------------------------------------------------------
postDriverClearFee ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.ClearDriverFeeReq ->
  Flow APISuccess
postDriverClearFee _merchantShortId _opCity driverId req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = DCommon.mapServiceName req.serviceName
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

---------------------------------------------------------------------
getDriverPanAadharSelfieDetails :: (ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow Common.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails merchantShortId _opCity countryCode mobileNumber = do
  merchant <- findMerchantByShortId merchantShortId
  hashedMobileNumber <- getDbHash mobileNumber
  let formattedCountryCode = "+" <> countryCode
  person <- QPerson.findByMobileNumberAndMerchantAndRole formattedCountryCode hashedMobileNumber merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound ("Person with number :" <> show mobileNumber <> " not found"))
  images <- QImage.findByPersonIdAndImageTypes person.id [DomainDVC.AadhaarCard, DomainDVC.PanCard, DomainDVC.ProfilePhoto]
  let sortedImages = sortOn (Down . (.updatedAt)) images
  let (profileImage, (aadhaarImage, panImage)) = sortedImages & (find ((== DomainDVC.ProfilePhoto) . (.imageType)) &&& find ((== DomainDVC.AadhaarCard) . (.imageType)) &&& find ((== DomainDVC.PanCard) . (.imageType)))
  return $ makePanAadharSelfieDetailsResp person profileImage aadhaarImage panImage
  where
    makePanAadharSelfieDetailsResp person profileImage aadhaarImage panImage =
      Common.PanAadharSelfieDetailsResp
        { aadhaarDetails = makeAadhaarDetails <$> aadhaarImage,
          panDetails = makePanDetails <$> panImage,
          personId = getId person.id,
          personName = person.firstName <> " " <> fromMaybe "" person.middleName <> " " <> fromMaybe "" person.lastName,
          selfieDetails = makeSelfieDetails <$> profileImage
        }

    makeAadhaarDetails DImage.Image {..} =
      Common.AadhaarDetails
        { aadhaarStatus = T.pack . show <$> verificationStatus,
          aadhaarStatusTime = updatedAt,
          aadhaarTransactionId = workflowTransactionId
        }

    makePanDetails DImage.Image {..} =
      Common.PanDetails
        { panStatus = T.pack . show <$> verificationStatus,
          panStatusTime = updatedAt,
          panTransactionId = workflowTransactionId
        }

    makeSelfieDetails DImage.Image {..} =
      Common.SelfieDetails
        { latestStatus = T.pack . show <$> verificationStatus,
          latestStatusTime = updatedAt,
          latestTransactionId = workflowTransactionId
        }

---------------------------------------------------------------------
postDriverSyncDocAadharPan :: (ShortId DM.Merchant -> Context.City -> Common.AadharPanSyncReq -> Flow APISuccess)
postDriverSyncDocAadharPan merchantShortId _opCity Common.AadharPanSyncReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  hashedMobileNumber <- getDbHash phoneNo
  person <- QPerson.findByMobileNumberAndMerchantAndRole ("+" <> countryCode) hashedMobileNumber merchant.id DP.DRIVER >>= fromMaybeM (PersonNotFound ("Person with number :" <> show phoneNo <> " not found"))
  images <- QImage.findRecentLatestByPersonIdAndImagesType person.id (convertDocTypeToDVCDocType documentType)
  case documentType of
    Common.Aadhaar -> do
      imgs <- extract2 images
      aadhaarDetails <- B.runInReplica $ QAadhaarCard.findByPrimaryKey person.id
      case aadhaarDetails of
        Nothing -> void $ mapM (maybe (return ()) (QImage.deleteById . (.id))) imgs
        Just aadhaar -> do
          if uncurry (&&) $ ((aadhaar.aadhaarFrontImageId `elem`) &&& (aadhaar.aadhaarBackImageId `elem`)) [img <&> (.id) | img <- imgs]
            then throwError DocumentAlreadyInSync
            else void $ mapM (maybe (return ()) (QImage.deleteById . (.id))) imgs
    Common.Pan -> do
      image <- fromMaybeM UnsyncedImageNotFound (listToMaybe images)
      when (isNothing image.workflowTransactionId) $ throwError NotValidatedUisngFrontendSDK
      when (length images > 1) $ throwError (InternalError "More than one Image found for document type PAN which is not possible using frontend sdk flow!!!!!!!!!")

      panDetails <- B.runInReplica $ QPanCard.findByDriverId person.id
      case panDetails of
        Nothing -> QImage.deleteById image.id
        Just pan -> do
          if pan.documentImageId1 /= image.id
            then do
              QImage.deleteById image.id
            else throwError DocumentAlreadyInSync
  return Success
  where
    convertDocTypeToDVCDocType Common.Aadhaar = DomainDVC.AadhaarCard
    convertDocTypeToDVCDocType Common.Pan = DomainDVC.PanCard

    extract2 (x : y : []) = return [Just x, Just y]
    extract2 (x : []) = return [Just x, Nothing]
    extract2 _ = throwError (InternalError "No Image found for document type Aadhaar!!!!!!!!!")
