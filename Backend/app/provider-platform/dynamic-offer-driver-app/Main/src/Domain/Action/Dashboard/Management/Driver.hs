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
    getDriverAadhaarInfo,
    getDriverAadhaarInfobyMobileNumber,
    getDriverList,
    getDriverActivity,
    postDriverDisable,
    postDriverPersonNumbers,
    postDriverPersonId,
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
    postDriverUpdateVehicleManufacturing,
    postDriverRefundByPayout,
    getDriverSecurityDepositStatus,
    postDriverDriverDataDecryption,
    getDriverPanAadharSelfieDetailsList,
    postDriverBulkSubscriptionServiceUpdate,
    getDriverStats,
    checkDriverOperatorAssociation,
    checkFleetOperatorAssociation,
    checkFleetDriverAssociation,
    getDriverEarnings,
    isAssociationBetweenTwoPerson,
    postDriverUpdateTagBulk,
    postDriverUpdateMerchant,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Driver as Common
import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Csv
import qualified Data.HashSet as HS
import Data.List (nub, partition, sortOn)
import Data.List.NonEmpty (nonEmpty)
import Data.List.Split (chunksOf)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Vector as V
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Driver.Notification as DDN
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AVD
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DocumentVerificationConfig as DomainDVC
import qualified Domain.Types.DriverBlockReason as DBR
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverLicense
import qualified Domain.Types.DriverPlan as DDPlan
import Domain.Types.DriverRCAssociation
import qualified Domain.Types.HyperVergeSdkLogs as DomainHVSdkLogs
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
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
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DeleteDriver as DeleteDriver
import SharedLogic.DriverOnboarding
import SharedLogic.DriverOnboarding.Status (ResponseStatus (..))
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified SharedLogic.EventTracking as SEVT
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.Ride
import SharedLogic.VehicleServiceTier
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC
import Storage.CachedQueries.DriverBlockReason as DBR
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOperatorAssociation as QDriverOperator
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.DriverProfileQuestions as QDriverProfileQuestions
import qualified Storage.Queries.DriverRCAssociation as QDriverRCAssociation
import qualified Storage.Queries.DriverReferral as QDriverReferral
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperator
import qualified Storage.Queries.HyperVergeSdkLogs as QHyperVergeSdkLogs
import qualified Storage.Queries.HyperVergeVerification as QHyperVergeVerification
import qualified Storage.Queries.IdfyVerification as QIdfyVerification
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

getLicenseStatus :: Int -> Int -> Maybe DriverLicense -> Maybe IV.IdfyVerification -> SStatus.ResponseStatus
getLicenseStatus onboardingTryLimit currentTries mbLicense mbLicReq =
  case mbLicense of
    Just driverLicense -> SStatus.mapStatus driverLicense.verificationStatus
    Nothing -> verificationState onboardingTryLimit currentTries mbLicReq

getRegCertStatus :: Int -> Int -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate) -> Maybe IV.IdfyVerification -> SStatus.ResponseStatus
getRegCertStatus onboardingTryLimit currentTries mbRegCert mbVehRegReq =
  case mbRegCert of
    Just (_assoc, vehicleRC) -> SStatus.mapStatus vehicleRC.verificationStatus
    Nothing -> verificationState onboardingTryLimit currentTries mbVehRegReq

verificationState :: Int -> Int -> Maybe IV.IdfyVerification -> SStatus.ResponseStatus
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
getDriverList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.DriverListRes
getDriverList merchantShortId opCity mbLimit mbOffset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhone mbVehicleNumberSearchString mbNameSearchString = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  driversWithInfo <- B.runInReplica $ QPerson.findAllDriversWithInfoAndVehicle merchant merchantOpCity limit offset mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString mbNameSearchString
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
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig driverId False Nothing
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

  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
  checkAndUpdateAirConditioned True req.isWorking personId cityVehicleServiceTiers req.downgradeReason
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
  QDriverInfo.updateDynamicBlockedStateWithActivity driverId req.blockReason req.blockTimeInHours dashboardUserName merchantId req.reasonCode driver.merchantOperatingCityId DTDBT.Dashboard True Nothing Nothing ByDashboard
  case req.blockTimeInHours of
    Just hrs -> do
      let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral hrs) * 60 * 60
      JC.createJobIn @_ @'UnblockDriver (Just merchantId) (Just merchantOpCityId) unblockDriverJobTs $
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
  when (not driverInf.blocked) (void $ QDriverInfo.updateBlockedState driverId True Nothing merchantId driver.merchantOperatingCityId DTDBT.Dashboard)
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
postDriverUnblock :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Maybe UTCTime -> Maybe UTCTime -> Flow APISuccess
postDriverUnblock merchantShortId opCity reqDriverId dashboardUserName preventWeeklyCancellationRateBlockingTill preventDailyCancellationRateBlockingTill = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  driverInf <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInf.blocked $ do
    QDriverInfo.updateBlockedState driverId False (Just dashboardUserName) merchantId driver.merchantOperatingCityId DTDBT.Dashboard
    now <- getCurrentTime
    void $ LF.blockDriverLocationsTill (driver.merchantId) (driver.id) now -- this will eventually unblock driver locations as block till is set to now
    case (preventDailyCancellationRateBlockingTill, preventWeeklyCancellationRateBlockingTill) of
      (Just _, Just _) -> do
        QDriverInfo.updateDailyAndWeeklyCancellationRateBlockingCooldown preventDailyCancellationRateBlockingTill preventWeeklyCancellationRateBlockingTill driver.id
      (Just _, Nothing) -> do
        QDriverInfo.updateDailyCancellationRateBlockingCooldown preventDailyCancellationRateBlockingTill driver.id
      (Nothing, Just _) -> do
        QDriverInfo.updateWeeklyCancellationRateBlockingCooldown preventWeeklyCancellationRateBlockingTill driver.id
      _ -> pure ()
  when (isJust driverInf.softBlockStiers) $ do
    QDriverInfo.updateSoftBlock Nothing Nothing Nothing (cast driverId)
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

-- FIXME remove this, all entities should be limited on db level
limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

---------------------------------------------------------------------
deleteDriverPermanentlyDelete :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
deleteDriverPermanentlyDelete merchantShortId _ = DeleteDriver.deleteDriver merchantShortId . cast

---------------------------------------------------------------------
postDriverUnlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverUnlinkDL merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let driverId_ = cast @Common.Driver @DP.Driver driverId
  let personId = cast @Common.Driver @DP.Person driverId

  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  QDriverLicense.deleteByDriverId personId
  Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig driverId_ False (Just False)
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
  unless (transporterConfig.aadhaarVerificationRequired) $ Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig driverId_ False (Just False)
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

postDriverUpdateTagBulk :: ShortId DM.Merchant -> Context.City -> Common.UpdateTagBulkReq -> Flow [Dashboard.Common.UpdateTagBulkRes]
postDriverUpdateTagBulk merchantShortId opCity req = do
  csvData <- liftIO $ BS.readFile req.file
  case decodeByName (LBS.fromStrict csvData) :: Either String (V.Vector BS.ByteString, V.Vector Dashboard.Common.DriverTagBulkCSVRow) of
    Left err -> do
      logInfo $ "CSV parse error: " <> show err
      return [Dashboard.Common.UpdateTagBulkRes "parse-error" False (Just $ T.pack err)]
    Right (_, v) -> do
      results <- forM (V.toList v) $ \row -> do
        res <- withTryCatch "processDriverTagUpdate" (processDriverTagUpdate merchantShortId opCity row)
        case res of
          Left err -> do
            let errorMsg = show err
            logInfo $ "Error processing driver " <> row.driverId <> ": " <> T.pack errorMsg
            return $ Dashboard.Common.UpdateTagBulkRes row.driverId False (Just $ T.pack errorMsg)
          Right _ -> do
            logInfo $ "Successfully processed driver " <> row.driverId
            return $ Dashboard.Common.UpdateTagBulkRes row.driverId True Nothing
      return results
  where
    processDriverTagUpdate :: ShortId DM.Merchant -> Context.City -> Dashboard.Common.DriverTagBulkCSVRow -> Flow ()
    processDriverTagUpdate merchantShortId' opCity' row = do
      merchant <- findMerchantByShortId merchantShortId'
      merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity')

      -- Convert driverId to Person ID
      let personId = Id row.driverId
      driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

      -- Check if driver has a valid merchant ID
      when (driver.merchantId == Id "") $
        throwError (InvalidRequest $ "Driver " <> row.driverId <> " has no merchant ID")

      -- Merchant access checking
      unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $
        throwError (PersonDoesNotExist personId.getId)

      -- Create tag name value using the same pattern as postDriverUpdateDriverTag
      -- Use mkTagNameValue to ensure proper tag creation
      let tagName = LYT.TagName row.tagName
      let tagValue = LYT.TextValue row.tagValue
      let tagNameValue = Yudhishthira.mkTagNameValue tagName tagValue

      -- Check if tag already exists
      when (maybe False (Yudhishthira.elemTagNameValue tagNameValue) driver.driverTag) $
        logInfo "Tag already exists, updating expiry"

      -- Verify tag
      mbNammTag <- Yudhishthira.verifyTag tagNameValue
      now <- getCurrentTime

      -- Update tag (always add/replace)
      let reqDriverTagWithExpiry = Yudhishthira.addTagExpiry tagNameValue (mbNammTag >>= (.validity)) now
      let tag = Yudhishthira.replaceTagNameValue driver.driverTag reqDriverTagWithExpiry

      -- Update database if tag changed
      unless (Just (Yudhishthira.showRawTags tag) == (Yudhishthira.showRawTags <$> driver.driverTag)) $ do
        QPerson.updateDriverTag (Just tag) personId
        logInfo $ "Updated tag for driver " <> row.driverId <> ": " <> show tagNameValue

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
          void $ LF.rideDetails dI.ride.id SRide.CANCELLED merchant.id dI.ride.driverId dI.ride.fromLocation.lat dI.ride.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (dI.ride.fromLocation.lat) (dI.ride.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) dI.ride.stops}))
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
  mVehicle <- QVehicle.findById personId
  let vehicleCategory = mVehicle >>= (.category)
  case (driverPlan, req.planId) of
    (Just dp, Just planId) -> do
      if dp.planId == Id planId
        then do
          void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId vehicleCategory
        else do
          void $ DTPlan.planSwitch serviceName (Id planId) (driver.id, driver.merchantId, driver.merchantOperatingCityId)
          void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId vehicleCategory
    (Nothing, Just _) -> do
      void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId vehicleCategory
    (Just dp, Nothing) -> do
      transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let enableServiceUsageCharge = dp.enableServiceUsageCharge
      when (enableServiceUsageCharge /= req.serviceChargeEligibility) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName req.serviceChargeEligibility personId serviceName
        fork "track service toggle" $ do
          SEVT.trackServiceUsageChargeToggle dp (show <$> req.reason)
        when (serviceName == YATRI_RENTAL) $ do
          fork "notify rental event" $ do
            DCommon.notifyYatriRentalEventsToDriver req.vehicleId (getMkeyForEvent req.serviceChargeEligibility) personId transporterConfig (show <$> req.reason) WHATSAPP
    (Nothing, Nothing) -> throwError $ InvalidRequest "pls provide a plan Id to enable subscription"
  pure Success
  where
    getMkeyForEvent serviceChargeEligiblity = if serviceChargeEligiblity then YATRI_RENTAL_RESUME else YATRI_RENTAL_PAUSE

toggleDriverSubscriptionByService ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe (Id Plan) ->
  Bool ->
  Maybe Text ->
  Maybe DVC.VehicleCategory ->
  Flow ()
toggleDriverSubscriptionByService (driverId, mId, mOpCityId) serviceName mbPlanToAssign toToggle mbVehicleNo mbVehicleCategory = do
  (autoPayStatus, driverPlan) <- DTPlan.getSubcriptionStatusWithPlan serviceName driverId
  transporterConfig <- CTC.findByMerchantOpCityId mOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
  if toToggle
    then do
      planToAssign <- getPlanId mbPlanToAssign
      case autoPayStatus of
        Just DrInfo.ACTIVE -> pure ()
        _ -> callSubscribeFlowForDriver planToAssign
      whenJust mbVehicleNo $ \vehicleNo -> do
        QDP.updatesubscriptionServiceRelatedDataInDriverPlan driverId (DDPlan.RentedVehicleNumber vehicleNo) serviceName
      QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
      fork "notify rental event" $ do
        DCommon.notifyYatriRentalEventsToDriver mbVehicleNo WHATSAPP_VEHICLE_LINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
    else do
      when (isJust driverPlan) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
        fork "track service toggle" $ do
          case driverPlan of
            Just dp -> SEVT.trackServiceUsageChargeToggle dp Nothing
            Nothing -> pure ()
        fork "notify rental event" $ do
          DCommon.notifyYatriRentalEventsToDriver mbVehicleNo WHATSAPP_VEHICLE_UNLINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
  where
    getPlanId :: Maybe (Id Plan) -> Flow (Id Plan)
    getPlanId mbPlanId = do
      case mbPlanId of
        Nothing -> do
          plans <- maybe (pure []) (\vc -> CQP.findByMerchantOpCityIdAndTypeWithServiceName mOpCityId DEFAULT serviceName vc False) mbVehicleCategory
          case plans of
            [] -> throwError $ InternalError "No default plans found"
            [pl] -> pure pl.id
            _ -> throwError $ InternalError "Multiple default plans found"
        Just planId -> pure planId
    callSubscribeFlowForDriver :: Id Plan -> Flow ()
    callSubscribeFlowForDriver planId = do
      let serviceSpecificData = maybe DDPlan.NoData DDPlan.RentedVehicleNumber mbVehicleNo
      void $ DTPlan.planSubscribe serviceName planId (True, Just WHATSAPP) (cast driverId, mId, mOpCityId) serviceSpecificData
      pure ()

---------------------------------------------------------------------
postDriverUpdateRCInvalidStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateRCInvalidStatusReq -> Flow APISuccess
postDriverUpdateRCInvalidStatus _merchantShortId _opCity _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  RCQuery.updateVehicleVariant vehicleRC.id (Just req.vehicleVariant) Nothing (Just True)
  pure Success

---------------------------------------------------------------------
postDriverUpdateVehicleVariant :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateVehicleVariantReq -> Flow APISuccess
postDriverUpdateVehicleVariant _merchantShortId _opCity _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  rcNumber <- decrypt vehicleRC.certificateNumber
  mVehicle <- QVehicle.findByRegistrationNo rcNumber
  RCQuery.updateVehicleVariant vehicleRC.id (Just req.vehicleVariant) Nothing Nothing
  whenJust mVehicle $ \vehicle -> updateVehicleVariantAndServiceTier req.vehicleVariant vehicle $ DV.castVehicleVariantToVehicleCategory req.vehicleVariant
  pure Success

updateVehicleVariantAndServiceTier :: DV.VehicleVariant -> DVeh.Vehicle -> DVC.VehicleCategory -> Flow ()
updateVehicleVariantAndServiceTier variant vehicle vehicleCategory = do
  driver <- B.runInReplica $ QPerson.findById vehicle.driverId >>= fromMaybeM (PersonDoesNotExist vehicle.driverId.getId)
  driverInfo' <- QDriverInfo.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  -- driverStats <- runInReplica $ QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId driver.merchantOperatingCityId (Just [])
  let availableServiceTiersForDriver = (.serviceTierType) . fst <$> selectVehicleTierForDriverWithUsageRestriction True driverInfo' vehicle vehicleServiceTiers
  QVehicle.updateVariantAndServiceTiers variant availableServiceTiersForDriver (Just vehicleCategory) vehicle.driverId

---------------------------------------------------------------------
postDriverBulkReviewRCVariant :: ShortId DM.Merchant -> Context.City -> [Common.ReviewRCVariantReq] -> Flow [Common.ReviewRCVariantRes]
postDriverBulkReviewRCVariant _ _ req = do
  mapM
    ( \rcReq -> do
        res <- withTryCatch "processRCReq" (processRCReq rcReq)
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
        whenJust rcReq.vehicleVariant $ \variant -> updateVehicleVariantAndServiceTier variant vehicle $ DV.castVehicleVariantToVehicleCategory variant

---------------------------------------------------------------------
postDriverUpdateDriverTag :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverTagReq -> Flow APISuccess
postDriverUpdateDriverTag merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (req.isAddingTag && maybe False (Yudhishthira.elemTagNameValue req.driverTag) driver.driverTag) $
    logInfo "Tag already exists, update expiry"
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  mbNammTag <- Yudhishthira.verifyTag req.driverTag
  now <- getCurrentTime
  let tag =
        if req.isAddingTag
          then do
            let reqDriverTagWithExpiry = Yudhishthira.addTagExpiry req.driverTag (mbNammTag >>= (.validity)) now
            Yudhishthira.replaceTagNameValue driver.driverTag reqDriverTagWithExpiry
          else Yudhishthira.removeTagNameValue driver.driverTag req.driverTag
  unless (Just (Yudhishthira.showRawTags tag) == (Yudhishthira.showRawTags <$> driver.driverTag)) $
    QPerson.updateDriverTag (Just tag) personId
  pure Success

---------------------------------------------------------------------
postDriverClearFee :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.ClearDriverFeeReq -> Flow APISuccess
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
postDriverPersonId :: ShortId DM.Merchant -> Context.City -> Common.PersonMobileNoReq -> Flow [Common.PersonRes]
postDriverPersonId _ _ req = do
  csvData <- readCsvAndGetPersonIds req.file
  let chunks = chunksOf 100 csvData
  decryptedNumbers <- forM chunks processChunk
  return $ concat decryptedNumbers
  where
    readCsvAndGetPersonIds :: FilePath -> Flow [Text]
    readCsvAndGetPersonIds csvFile = do
      csvData <- liftIO $ BS.readFile csvFile
      case decodeByName (LBS.fromStrict csvData) :: Either String (V.Vector BS.ByteString, V.Vector Common.PersonMobileNumberIdsCsvRow) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> pure $ mapMaybe (.mobileNumber) $ V.toList v

    processChunk :: [Text] -> Flow [Common.PersonRes]
    processChunk chunk = do
      mobile <- QPerson.findPersonIdsByPhoneNumber chunk
      decryptedMobile <- forM mobile $ \p -> do
        decMobile <- decrypt p
        return $ Common.PersonRes decMobile.id.getId decMobile.mobileNumber decMobile.alternateMobileNumber decMobile.merchantOperatingCityId.getId
      return decryptedMobile

---------------------------------------------------------------------
getDriverPanAadharSelfieDetails :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow Common.PanAadharSelfieDetailsResp
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
postDriverSyncDocAadharPan :: ShortId DM.Merchant -> Context.City -> Common.AadharPanSyncReq -> Flow APISuccess
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

---------------------------------------------------------------------
postDriverUpdateVehicleManufacturing :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateVehicleManufacturingReq -> Flow APISuccess
postDriverUpdateVehicleManufacturing merchantShortId opCity reqDriverId Common.UpdateVehicleManufacturingReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  QVehicle.updateManufacturing (Just manufacturing) driverId
  RCQuery.updateManufacturing (Just manufacturing) (Id rcId)
  pure Success

---------------------------------------------------------------------
postDriverRefundByPayout :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RefundByPayoutReq -> Flow APISuccess
postDriverRefundByPayout merchantShortId _opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let refundByPayoutReq =
        DDriver.RefundByPayoutReq
          { serviceName = DCommon.mapServiceName req.serviceName,
            refundAmountDeduction = req.refundAmountDeduction,
            payerVpa = req.payerVpa,
            driverFeeType = mapFeeType req.driverFeeType,
            refundAmountSegregation = req.refundAmountSegregation
          }
  void $ DDriver.refundByPayoutDriverFee (personId, driver.merchantId, merchantOpCityId) refundByPayoutReq
  return Success

mapFeeType :: Common.DriverFeeType -> DDF.FeeType
mapFeeType Common.PAYOUT_REGISTRATION = DDF.PAYOUT_REGISTRATION
mapFeeType Common.ONE_TIME_SECURITY_DEPOSIT = DDF.ONE_TIME_SECURITY_DEPOSIT

---------------------------------------------------------------------
getDriverSecurityDepositStatus :: (ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Common.ServiceNames -> Flow [Common.SecurityDepositDfStatusRes])
getDriverSecurityDepositStatus merchantShortId _opCity driverId serviceName' = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = maybe YATRI_RENTAL DCommon.mapServiceName serviceName'
  response <- DDriver.getSecurityDepositDfStatus (personId, driver.merchantId, merchantOpCityId) serviceName
  return $ mapSecurityDepositDfStatusResToDashboardType response
  where
    mapSecurityDepositDfStatusResToDashboardType =
      map
        ( \(DDriver.SecurityDepositDfStatusRes {..}) ->
            Common.SecurityDepositDfStatusRes
              { securityDepositStatus = DCommon.castStatus securityDepositStatus,
                ..
              }
        )

---------------------------------------------------------------------
postDriverDriverDataDecryption :: ShortId DM.Merchant -> Context.City -> [Common.DriverEncDataReq] -> Flow [Common.DriverDecDataResp]
postDriverDriverDataDecryption _ _ = mapM decryptDriverData
  where
    toEncryptedHash :: Text -> Flow (EncryptedHashed Text)
    toEncryptedHash a = do
      let encrypted = Encrypted a
      hash <- getDbHash a
      pure $ EncryptedHashed {..}

    decryptDriverData :: Common.DriverEncDataReq -> Flow Common.DriverDecDataResp
    decryptDriverData encDriver = do
      let driverId = Common.driverIdReq encDriver
      encDriverData <- toEncryptedHash $ Common.driverDataReq encDriver
      decryptedDriverData <- decrypt encDriverData
      pure (Common.DriverDecDataResp driverId (Just decryptedDriverData))

---------------------------------------------------------------------
getDriverPanAadharSelfieDetailsList :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow [Common.PanAadharSelfieDetailsListResp]
getDriverPanAadharSelfieDetailsList merchantShortId _opCity docType' driverID = do
  let personId = cast @Common.Driver @DP.Person driverID
  merchant <- findMerchantByShortId merchantShortId
  documentType <- convertToDomainType docType'
  hvSdkLogs <- QHyperVergeSdkLogs.findAllByDriverIdAndDocType Nothing Nothing personId $ Just documentType
  imageDetails <- filter (isJust . (.workflowTransactionId)) <$> QImage.findImagesByPersonAndType Nothing Nothing merchant.id personId documentType
  let txnIdsInLogs = HS.fromList $ map (Just . (.txnId)) hvSdkLogs
      (imagesAvaialbleInLogs, imagesNotAvailableInLogs) = partition ((`HS.member` txnIdsInLogs) . (.workflowTransactionId)) imageDetails
  partialResp <- mapM (buildRespFromHVSdkLogs imagesAvaialbleInLogs) hvSdkLogs
  let uniqueTxnIdsNotInLogs = HS.toList . HS.fromList $ map (.workflowTransactionId) imagesNotAvailableInLogs
  remResp <- mapM (\txnId -> buildRemResp (filter ((== txnId) . (.workflowTransactionId)) imagesNotAvailableInLogs)) uniqueTxnIdsNotInLogs
  return $ partialResp <> remResp
  where
    buildRespFromHVSdkLogs :: (MonadThrow m, Log m) => [DImage.Image] -> DomainHVSdkLogs.HyperVergeSdkLogs -> m Common.PanAadharSelfieDetailsListResp
    buildRespFromHVSdkLogs imagesAvaialbleInLogs DomainHVSdkLogs.HyperVergeSdkLogs {..} = do
      (image1, image2) <- extractImages $ filter ((== Just txnId) . (.workflowTransactionId)) imagesAvaialbleInLogs
      return $
        Common.PanAadharSelfieDetailsListResp
          { transactionId = txnId,
            verificationStatus = convertToVerificationStatus <$> status,
            imageId1 = image1 <&> getId . (.id),
            imageId2 = image2 <&> getId . (.id),
            ..
          }
    buildRemResp :: (MonadThrow m, Log m) => [DImage.Image] -> m Common.PanAadharSelfieDetailsListResp
    buildRemResp imageList = do
      (image1, image2) <- extractImages imageList -- Here logically atleast one image should always come.
      createdAt <- fromMaybeM (InternalError "could not find created_at as Image does not exist !!!!!!") $ image1 <&> (.createdAt) -- these errors will never happen.
      updatedAt <- fromMaybeM (InternalError "could not find updated_at as Image does not exist !!!!!!") $ image1 <&> (.updatedAt) -- these errors will never happen.
      transactionId <- fromMaybeM (InternalError "could not find transactionId as Image does not exist !!!!!!") . join $ image1 <&> (.workflowTransactionId) -- these errors will never happen as all hyperverge workflow captured images have transactionId.
      return $
        Common.PanAadharSelfieDetailsListResp
          { verificationStatus = image1 >>= (show <$>) . (.verificationStatus),
            imageId1 = image1 <&> getId . (.id),
            imageId2 = image2 <&> getId . (.id),
            failureReason = Nothing,
            ..
          }

    extractImages :: (MonadThrow m, Log m) => [DImage.Image] -> m (Maybe DImage.Image, Maybe DImage.Image)
    extractImages [img1, img2] = return (Just img1, Just img2)
    extractImages [img1] = return (Just img1, Nothing)
    extractImages [] = return (Nothing, Nothing)
    extractImages _ = throwError $ InternalError "Document like Pan, aadhaar and selfie cannot have more than 2 images for single transactionId !!!!!!"

    convertToVerificationStatus :: Text -> Text
    convertToVerificationStatus = \case
      "auto_approved" -> "VALID"
      "auto_declined" -> "INVALID"
      "needs_review" -> "MANUAL_VERIFICATION_REQUIRED"
      "user_cancelled" -> "USER_CANCELLED"
      "error" -> "ERROR"
      _ -> "UNKNOWN"

    convertToDomainType :: (MonadThrow m, Log m) => Text -> m DomainDVC.DocumentType
    convertToDomainType docType = case docType of
      "PanCard" -> return DomainDVC.PanCard
      "AadhaarCard" -> return DomainDVC.AadhaarCard
      "ProfilePhoto" -> return DomainDVC.ProfilePhoto
      _ -> throwError $ InvalidDocumentType docType

postDriverBulkSubscriptionServiceUpdate :: ShortId DM.Merchant -> Context.City -> Common.BulkServiceUpdateReq -> Flow APISuccess
postDriverBulkSubscriptionServiceUpdate merchantShortId _opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  _ <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  when (length req.driverIds > 200) $ throwError (InvalidRequest "driver ids limit exceeded")
  let services = nub $ map DCommon.mapServiceName (req.serviceNames <> [Common.YATRI_SUBSCRIPTION])
  QDriverInfo.updateServicesEnabled req.driverIds services
  return Success

getDriverStats :: ShortId DM.Merchant -> Context.City -> Maybe (Id Common.Driver) -> Maybe Day -> Maybe Day -> Text -> Flow Common.DriverStatsRes
getDriverStats merchantShortId opCity mbEntityId mbFromDate mbToDate requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  whenJust mbEntityId $ \e_Id -> do
    let entityId = cast @Common.Driver @DP.Person e_Id
    when (entityId.getId /= requestorId) $ do
      entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, entityId] merchantOpCityId
      entity <- find (\e -> e.id == entityId) entities & fromMaybeM (PersonDoesNotExist entityId.getId)
      requestor <- find (\e -> e.id == Id requestorId) entities & fromMaybeM (PersonDoesNotExist requestorId)
      isValid <- isAssociationBetweenTwoPerson requestor entity
      unless isValid $ throwError AccessDenied

  let personId = cast @Common.Driver @DP.Person $ fromMaybe (Id requestorId) mbEntityId
  DDriver.findOnboardedDriversOrFleets personId merchantOpCityId mbFromDate mbToDate

isAssociationBetweenTwoPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DP.Person -> DP.Person -> m Bool
isAssociationBetweenTwoPerson requestedPersonDetails personDetails = do
  case (requestedPersonDetails.role, personDetails.role) of
    (DP.OPERATOR, DP.DRIVER) -> checkFleetDriverAndDriverOperatorAssociation personDetails.id requestedPersonDetails.id
    (DP.OPERATOR, DP.FLEET_OWNER) -> checkFleetOperatorAssociation personDetails.id requestedPersonDetails.id
    (DP.FLEET_OWNER, DP.DRIVER) -> checkFleetDriverAssociation requestedPersonDetails.id personDetails.id
    (DP.ADMIN, _) -> return True
    _ -> return False

checkFleetDriverAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetDriverAssociation fleetId driverId = do
  mbAssoc <- QFleetDriver.findByDriverIdAndFleetOwnerId driverId fleetId.getId True
  return $ isJust mbAssoc

checkFleetOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetOperatorAssociation fleetId operatorId = do
  mbAssoc <- QFleetOperator.findByFleetIdAndOperatorId fleetId.getId operatorId.getId True
  return $ isJust mbAssoc

checkDriverOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkDriverOperatorAssociation driverId operatorId = do
  mbAssoc <- QDriverOperator.findByDriverIdAndOperatorId driverId operatorId True
  return $ isJust mbAssoc

checkFleetDriverAndDriverOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkFleetDriverAndDriverOperatorAssociation driverId operatorId = do
  isDriverOperatorAssociated <- checkDriverOperatorAssociation driverId operatorId
  if isDriverOperatorAssociated
    then return True
    else do
      QFleetDriver.findByDriverId driverId True >>= \case
        Just fleetDriverAssoc -> checkFleetOperatorAssociation (Id fleetDriverAssoc.fleetOwnerId) operatorId
        Nothing -> return False

getDriverEarnings :: ShortId DM.Merchant -> Context.City -> Day -> Day -> Common.EarningType -> Id Common.Driver -> Text -> Flow Common.EarningPeriodStatsRes
getDriverEarnings merchantShortId opCity from to earningType dId requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Person dId
  entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, driverId] merchantOpCityId
  driver <- find (\e -> e.id == driverId) entities & fromMaybeM (PersonDoesNotExist driverId.getId)
  requestor <- find (\e -> e.id == Id requestorId) entities & fromMaybeM (PersonDoesNotExist requestorId)
  isValid <- isAssociationWithDriver requestor driver
  unless isValid $ throwError AccessDenied
  DDriver.getEarnings (driverId, merchant.id, merchantOpCityId) from to earningType
  where
    isAssociationWithDriver :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DP.Person -> DP.Person -> m Bool
    isAssociationWithDriver requestedPersonDetails driverDetails = do
      case (requestedPersonDetails.role, driverDetails.role) of
        (DP.OPERATOR, DP.DRIVER) -> checkDriverOperatorAssociation driverDetails.id requestedPersonDetails.id
        (DP.FLEET_OWNER, DP.DRIVER) -> checkFleetDriverAssociation requestedPersonDetails.id driverDetails.id
        _ -> return False

---------------------------------------------------------------------
-- Update merchant for a driver (handles duplicate driver migration)
postDriverUpdateMerchant :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverMerchantReq -> Flow APISuccess
postDriverUpdateMerchant merchantShortId _opCity reqDriverId req = do
  let personId = cast @Common.Driver @DP.Person reqDriverId
  let newMerchantId = cast @Dashboard.Common.Merchant @DM.Merchant req.newMerchantId
  let newMerchantOperatingCityId = cast @Dashboard.Common.MerchantOperatingCity @DMOC.MerchantOperatingCity req.newMerchantOperatingCityId
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Starting migration, merchantShortId: " <> show merchantShortId <> ", newMerchantId: " <> show newMerchantId <> ", newMerchantOperatingCityId: " <> show newMerchantOperatingCityId
  _ <- findMerchantByShortId merchantShortId
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Merchant found by shortId"
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Looking up person in Person table"
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Person found, currentMerchantId: " <> show person.merchantId <> ", hasMobileNumber: " <> show (isJust person.mobileNumber)
  let mobileNumberHash = person.mobileNumber <&> (.hash)
  when (person.merchantId == newMerchantId) $ do
    logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - ERROR: Driver already belongs to the target merchant"
    throwError $ InvalidRequest "Driver already belongs to the target merchant"
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Looking up DriverInformation for ny-bt entry"
  mbNyDriverInfo <- QDriverInfo.findById (cast personId)
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - DriverInfo lookup result for ny-bt: " <> show (isJust mbNyDriverInfo)
  nyDriverInfo <- fromMaybeM DriverInfoNotFound mbNyDriverInfo
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - DriverInfo found for ny-bt, enabled: " <> show nyDriverInfo.enabled
  let nyEnabled = nyDriverInfo.enabled
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Searching for duplicate person with same mobile and newMerchantId: " <> show newMerchantId
  mbDuplicatePerson <- case mobileNumberHash of
    Nothing -> do
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - No mobile number hash, skipping duplicate search"
      pure Nothing
    Just hash -> do
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Searching for duplicate with mobileNumberHash and merchantId: " <> show newMerchantId
      QPerson.findByMobileNumberAndMerchant hash newMerchantId
  logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Duplicate person search result: " <> show (fmap (.id) mbDuplicatePerson)
  case mbDuplicatePerson of
    Nothing -> do
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - No duplicate person found, nyEnabled: " <> show nyEnabled
      if nyEnabled
        then do
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 5: Migrating driver to new merchant (no duplicate found)"
          updateMerchantInAllTables personId newMerchantId newMerchantOperatingCityId
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 5: Migration completed successfully"
          pure Success
        else do
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - ERROR: Cannot migrate, driver not enabled and no duplicate exists"
          throwError $ InvalidRequest "Cannot migrate: Driver is not enabled and no duplicate entry exists"
    Just duplicatePerson -> do
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Duplicate person found with duplicateId: " <> show duplicatePerson.id <> ", duplicateMerchantId: " <> show duplicatePerson.merchantId
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Looking up DriverInformation for bt-bt entry, duplicateId: " <> show duplicatePerson.id
      mbBtDriverInfo <- QDriverInfo.findById (cast duplicatePerson.id)
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - DriverInfo lookup result for bt-bt (duplicateId: " <> show duplicatePerson.id <> "): " <> show (isJust mbBtDriverInfo)
      btDriverInfo <- fromMaybeM DriverInfoNotFound mbBtDriverInfo
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - DriverInfo found for bt-bt (duplicateId: " <> show duplicatePerson.id <> "), enabled: " <> show btDriverInfo.enabled
      let btEnabled = btDriverInfo.enabled
      logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Decision matrix: nyEnabled=" <> show nyEnabled <> ", btEnabled=" <> show btEnabled
      case (nyEnabled, btEnabled) of
        (True, True) -> do
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 1: Both enabled, returning error"
          throwError $ InvalidRequest "Cannot migrate: Both old and new merchant entries are enabled. We won't migrate it as the driver has already migrated to the new merchant."
        (False, False) -> do
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 2: Both disabled, returning error"
          throwError $ InvalidRequest "Cannot migrate: Both old and new merchant entries are disabled. When driver logs in he will be using the Bharat Taxi merchant automatically."
        (True, False) -> do
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 3: Deleting duplicate " <> show duplicatePerson.id <> " and migrating driver to new merchant"
          void $ DeleteDriver.deleteDriver merchantShortId duplicatePerson.id
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 3: Duplicate deleted, now updating all tables"
          updateMerchantInAllTables personId newMerchantId newMerchantOperatingCityId
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 3: Migration completed successfully"
          pure Success
        (False, True) -> do
          logTagInfo "postDriverUpdateMerchant" $ "driverId: " <> show personId <> " - Case 4: ny-bt disabled but bt-bt enabled, returning error"
          throwError $ InvalidRequest "Cannot migrate: Old merchant entry is disabled but new merchant entry is enabled. The new entry should be used."

-- Helper function to update merchant in all affected tables
-- NOTE: Person table is updated LAST to allow for retry on failure
-- (if Person is updated first and child tables fail, retry would be blocked by the merchantId check)
updateMerchantInAllTables :: Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
updateMerchantInAllTables personId merchantId merchantOperatingCityId = do
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - Starting update, newMerchantId: " <> show merchantId <> ", newMerchantOperatingCityId: " <> show merchantOperatingCityId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [1/15] Updating DriverInformation table"
  QDriverInfo.updateMerchantIdAndCityIdByDriverId personId merchantId merchantOperatingCityId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [2/15] Updating Vehicle table"
  QVehicle.updateMerchantIdAndCityIdByDriverId personId merchantId (Just $ getId merchantOperatingCityId)
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [3/15] Updating DailyStats table"
  QDailyStats.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [4/15] Updating AadhaarCard table"
  QAadhaarCard.updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [5/15] Updating IdfyVerification table"
  QIdfyVerification.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [6/15] Updating Image table"
  QImage.updateMerchantIdAndCityIdByPersonId merchantId (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [7/15] Updating DriverReferral table"
  QDriverReferral.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [8/15] Updating DriverLicense table"
  QDriverLicense.updateMerchantIdByDriverId (Just merchantId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [9/15] Updating DriverPanCard table"
  QPanCard.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [10/15] Updating DriverPlan table"
  QDP.updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [11/15] Updating DriverProfileQuestions table"
  QDriverProfileQuestions.updateMerchantOperatingCityIdByDriverId merchantOperatingCityId personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [12/15] Updating DriverRCAssociation table"
  QDriverRCAssociation.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [13/15] Updating HyperVergeSdkLogs table"
  QHyperVergeSdkLogs.updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [14/15] Updating HyperVergeVerification table"
  QHyperVergeVerification.updateMerchantIdAndCityIdByDriverId (Just merchantId) (Just merchantOperatingCityId) personId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [15/15] Fetching RC associations for VehicleRegistrationCertificate update"
  rcAssociations <- QDriverRCAssociation.findAllByDriverId personId
  let rcCount = length rcAssociations
  let rcIds = map (\(assoc, _) -> assoc.rcId) rcAssociations
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [15/15] Found " <> show rcCount <> " RC associations, rcIds: " <> show rcIds
  forM_ rcAssociations $ \(assoc, _) -> do
    logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [15/15] Updating VehicleRegistrationCertificate for rcId: " <> show assoc.rcId
    RCQuery.updateMerchantIdAndCityIdById (Just merchantId) (Just merchantOperatingCityId) assoc.rcId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - [FINAL] Updating Person table"
  QPerson.updateMerchantIdAndCityId personId merchantId merchantOperatingCityId
  logTagInfo "updateMerchantInAllTables" $ "driverId: " <> show personId <> " - Successfully updated merchant in all tables"
