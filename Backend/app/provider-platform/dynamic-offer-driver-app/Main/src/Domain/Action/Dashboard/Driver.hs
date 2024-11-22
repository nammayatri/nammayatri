{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.Driver
  ( driverDocumentsInfo,
    driverAadhaarInfo,
    getDriverPersonNumbers,
    getDriverPersonId,
    listDrivers,
    driverActivity,
    enableDriver,
    disableDriver,
    updateACUsageRestriction,
    blockDriverWithReason,
    blockDriver,
    blockReasonList,
    unblockDriver,
    driverLocation,
    driverInfo,
    deleteDriver,
    unlinkVehicle,
    unlinkDL,
    unlinkAadhaar,
    endRCAssociation,
    updatePhoneNumber,
    addVehicle,
    updateDriverName,
    clearOnRideStuckDrivers,
    getDriverDue,
    collectCash,
    collectCashV2,
    exemptCashV2,
    exemptCash,
    driverAadhaarInfoByPhone,
    updateByPhoneNumber,
    setRCStatus,
    deleteRC,
    getDriverHomeLocation,
    updateDriverHomeLocation,
    incrementDriverGoToCount,
    getDriverGoHomeInfo,
    getPaymentHistoryEntityDetails,
    getPaymentHistory,
    addVehicleForFleet,
    getAllVehicleForFleet,
    fleetUnlinkVehicle,
    fleetRemoveVehicle,
    fleetTotalEarning,
    fleetVehicleEarning,
    fleetDriverEarning,
    setVehicleDriverRcStatusForFleet,
    fleetRemoveDriver,
    getFleetDriverVehicleAssociation,
    getFleetDriverAssociation,
    getFleetVehicleAssociation,
    getAllDriverForFleet,
    VolunteerTransactionStorageReq (..),
    setServiceChargeEligibleFlagInDriverPlan,
    addVehiclesInFleet,
    changeOperatingCity,
    getOperatingCity,
    updateRCInvalidStatus,
    updateVehicleVariant,
    bulkReviewRCVariant,
    updateDriverTag,
    registerRCForFleetWithoutDriver,
    updateFleetOwnerInfo,
    getFleetOwnerInfo,
    linkRCWithDriverForFleet,
    postDriverClearFee,
    getDriverPanAadharSelfieDetails,
    postDriverSyncDocAadharPan,
    postDriverUpdateVehicleManufacturing,
    postDriverRefundByPayout,
    getDriverSecurityDepositStatus,
    postDriverDriverDataDecryption,
    getDriverPanAadharSelfieDetailsList,
    mapServiceName,
  )
where

import qualified "this" API.Types.Dashboard.RideBooking.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as DC
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverGoHome as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import Control.Applicative ((<|>))
import "dashboard-helper-api" Dashboard.Common (HideSecrets (hideSecrets))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Csv
import qualified Data.HashSet as HS
import Data.List (partition, sortOn)
import Data.List.NonEmpty (nonEmpty)
import Data.List.Split (chunksOf)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Vector as V
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Action.UI.Driver as Driver
import Domain.Action.UI.DriverGoHomeRequest (CachedGoHomeRequest (..))
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AVD
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Plan as DTPlan
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.Common as DrInfo
import qualified Domain.Types.DocumentVerificationConfig as DomainDVC
import qualified Domain.Types.DriverBlockReason as DBR
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverHomeLocation as DDHL
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverLicense
import qualified Domain.Types.DriverPlan as DDPlan
import Domain.Types.DriverRCAssociation
import Domain.Types.FleetDriverAssociation
import qualified Domain.Types.FleetDriverAssociation as DTFDA
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.HyperVergeSdkLogs as DomainHVSdkLogs
import qualified Domain.Types.IdfyVerification as IV
import Domain.Types.Image (Image)
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage (MediaChannel (..), MessageKey (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as SRide
import Domain.Types.TransporterConfig
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DV
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import qualified Lib.Yudhishthira.Flow.Dashboard as Yudhishthira
import SharedLogic.Allocator
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import qualified SharedLogic.DeleteDriver as DeleteDriver
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import qualified SharedLogic.EventTracking as SEVT
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Ride
import SharedLogic.VehicleServiceTier
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQGHC
import Storage.CachedQueries.DriverBlockReason as DBR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Clickhouse.Ride as CQRide
import Storage.Clickhouse.RideDetails (findIdsByFleetOwner)
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverBlockTransactions as QDBT
import Storage.Queries.DriverFee (findPendingFeesByDriverIdAndServiceName)
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverHomeLocation as QDHL
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverPanCard as DPC
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverRCAssociationExtra as DRCAE
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociation as FDV
import qualified Storage.Queries.FleetOwnerInformation as FOI
import Storage.Queries.FleetRCAssociationExtra as FRAE
import qualified Storage.Queries.HyperVergeSdkLogs as QSdkLogs
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.RegistrationToken as QR
import Storage.Queries.Ride as QRide
import qualified Storage.Queries.Status as QDocStatus
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import qualified Tools.Auth as Auth
import Tools.Error
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp
import Utils.Common.Cac.KeyNameConstants

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
getDriverDue :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Text -> Flow [Common.DriverOutstandingBalanceResp] -- add mig and totalFee
getDriverDue merchantShortId _ mbMobileCountryCode phone = do
  let mobileCountryCode = fromMaybe "+91" mbMobileCountryCode
  merchant <- findMerchantByShortId merchantShortId
  mobileNumber <- getDbHash phone
  driver <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumber merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
  driverFees <- findPendingFeesByDriverIdAndServiceName (cast driver.id) Nothing YATRI_SUBSCRIPTION
  driverFeeByInvoices <- case driverFees of
    [] -> pure []
    driverFee : _ -> SLDriverFee.groupDriverFeeByInvoices driverFee.currency driverFees
  return $ map (mkPaymentDueResp driver.id) driverFeeByInvoices
  where
    mkPaymentDueResp driverId SLDriverFee.DriverFeeByInvoice {..} = do
      let platformFee_ = mkPlatformFee platformFee
          status_ = castStatus status
          driverFeeId = cast invoiceId
          driverId_ = cast driverId
      Common.DriverOutstandingBalanceResp
        { govtCharges = round govtCharges,
          govtChargesWithCurrency = PriceAPIEntity govtCharges currency,
          platformFee = platformFee_,
          status = status_,
          driverId = driverId_,
          totalFee = roundToIntegral totalFee,
          totalEarnings = roundToIntegral totalEarnings,
          totalFeeWithCurrency = PriceAPIEntity totalFee currency,
          totalEarningsWithCurrency = PriceAPIEntity totalEarnings currency,
          ..
        }

    mkPlatformFee SLDriverFee.PlatformFee {..} =
      Common.PlatformFee
        { feeWithCurrency = PriceAPIEntity fee currency,
          cgstWithCurrency = PriceAPIEntity cgst currency,
          sgstWithCurrency = PriceAPIEntity sgst currency,
          ..
        }

castStatus :: DriverFeeStatus -> Common.DriverFeeStatus
castStatus status = case status of -- only PENDING and OVERDUE possible
  ONGOING -> Common.ONGOING
  PAYMENT_PENDING -> Common.PAYMENT_PENDING
  PAYMENT_OVERDUE -> Common.PAYMENT_OVERDUE
  CLEARED -> Common.CLEARED
  EXEMPTED -> Common.EXEMPTED
  COLLECTED_CASH -> Common.COLLECTED_CASH
  INACTIVE -> Common.INACTIVE
  CLEARED_BY_YATRI_COINS -> Common.CLEARED_BY_YATRI_COINS
  MANUAL_REVIEW_NEEDED -> Common.MANUAL_REVIEW_NEEDED
  REFUND_PENDING -> Common.REFUND_PENDING
  REFUNDED -> Common.REFUNDED
  REFUND_FAILED -> Common.REFUND_FAILED
  REFUND_MANUAL_REVIEW_REQUIRED -> Common.REFUND_MANUAL_REVIEW_REQUIRED
  ONE_TIME_SECURITY_ADJUSTED -> Common.ONE_TIME_SECURITY_ADJUSTED
  SETTLED -> Common.SETTLED

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
enableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
enableDriver merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  mVehicle <- QVehicle.findById personId
  linkedRCs <- QRCAssociation.findAllLinkedByDriverId personId
  mbDriverLicense <- QDL.findByDriverId driverId

  when ((isNothing mVehicle && null linkedRCs) || isNothing mbDriverLicense) $
    throwError (InvalidRequest "Can't enable driver if no vehicle or no RCs or no DL are linked to them")

  enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId driverId False
  logTagInfo "dashboard -> enableDriver : " (show personId)
  fork "sending dashboard sms - onboarding" $ do
    Sms.sendDashboardSms merchant.id merchantOpCityId Sms.ONBOARDING Nothing personId Nothing 0
  pure Success

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
  checkAndUpdateAirConditioned True req.isWorking personId cityVehicleServiceTiers req.downgradeReason
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
  when (not driverInf.blocked) (void $ QDriverInfo.updateBlockedState driverId True Nothing merchantId driver.merchantOperatingCityId DTDBT.Dashboard)
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

---------------------------------------------------------------------
collectCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
collectCash mId city driver requestorId = recordPayment False mId city driver requestorId YATRI_SUBSCRIPTION Nothing

collectCashV2 :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.ServiceNames -> Maybe Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
collectCashV2 mId city driver requestorId serviceName mbExemptionAndCashCollectionDriverFeeReq = recordPayment False mId city driver requestorId (mapServiceName serviceName) mbExemptionAndCashCollectionDriverFeeReq

---------------------------------------------------------------------

exemptCash :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow APISuccess
exemptCash mId city driver requestorId = recordPayment True mId city driver requestorId YATRI_SUBSCRIPTION Nothing

exemptCashV2 :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.ServiceNames -> Maybe Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
exemptCashV2 mId city driver requestorId serviceName mbExemptionAndCashCollectionDriverFeeReq = recordPayment True mId city driver requestorId (mapServiceName serviceName) mbExemptionAndCashCollectionDriverFeeReq

mapServiceName :: Common.ServiceNames -> ServiceNames
mapServiceName common = case common of
  Common.YATRI_SUBSCRIPTION -> YATRI_SUBSCRIPTION
  Common.YATRI_RENTAL -> YATRI_RENTAL

---------------------------------------------------------------------

paymentStatus :: Bool -> DriverFeeStatus
paymentStatus isExempted
  | isExempted = EXEMPTED
  | otherwise = COLLECTED_CASH

recordPaymentLockKey :: Id Common.Driver -> Text
recordPaymentLockKey driverId = "RP:LK:DId:-" <> driverId.getId

recordPayment :: Bool -> ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> ServiceNames -> Maybe Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
recordPayment isExempted merchantShortId opCity reqDriverId requestorId serviceName mbExemptionAndCashCollectionDriverFeeReq = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica (QPerson.findById personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  Redis.whenWithLockRedis (recordPaymentLockKey reqDriverId) 30 $ do
    -- merchant access checking
    let merchantId = driver.merchantId
    unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
    driverFees <-
      case mbExemptionAndCashCollectionDriverFeeReq of
        Nothing -> findPendingFeesByDriverIdAndServiceName driverId Nothing serviceName
        Just req -> do
          duePaymentIds <- findPendingFeesByDriverIdAndServiceName driverId (Just $ map Id req.paymentIds) serviceName
          let len = length duePaymentIds
          let paymentIdLength = length req.paymentIds
          unless (len == paymentIdLength) $
            throwError $ InvalidRequest "Status of some id is not PAYMENT_OVERDUE."
          return duePaymentIds
    let totalFee = sum $ map (\fee -> fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst) driverFees
    transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    QDriverInfo.updatePendingPayment False driverId
    QDriverInfo.updateSubscription True driverId
    mapM_ (QDF.updateCollectedPaymentStatus (paymentStatus isExempted) (Just requestorId) now) ((.id) <$> driverFees)
    invoices <- (B.runInReplica . QINV.findActiveManualOrMandateSetupInvoiceByFeeId . (.id)) `mapM` driverFees
    mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) (concat invoices)
    unless isExempted $ do
      mapM_
        ( \dFee -> do
            invoice <- mkInvoice dFee
            QINV.create invoice
        )
        driverFees
    fork "sending dashboard sms - collected cash" $ do
      Sms.sendDashboardSms merchantId merchantOpCityId Sms.CASH_COLLECTED Nothing personId Nothing totalFee
  pure Success
  where
    mkInvoice driverFee = do
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      return $
        INV.Invoice
          { id = Id id,
            invoiceShortId = shortId.getShortId,
            driverFeeId = driverFee.id,
            invoiceStatus = INV.SUCCESS,
            driverId = driverFee.driverId,
            maxMandateAmount = Nothing,
            paymentMode = INV.CASH_COLLECTED_INVOICE,
            bankErrorCode = Nothing,
            bankErrorMessage = Nothing,
            bankErrorUpdatedAt = Nothing,
            lastStatusCheckedAt = Nothing,
            serviceName = driverFee.serviceName,
            merchantId = Just driverFee.merchantId,
            merchantOperatingCityId = driverFee.merchantOperatingCityId,
            updatedAt = now,
            createdAt = now
          }

---------------------------------------------------------------------
unblockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Maybe UTCTime -> Maybe UTCTime -> Flow APISuccess
unblockDriver merchantShortId opCity reqDriverId dashboardUserName preventWeeklyCancellationRateBlockingTill preventDailyCancellationRateBlockingTill = do
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
    void $ LTS.blockDriverLocationsTill (driver.merchantId) (driver.id) now -- this will eventually unblock driver locations as block till is set to now
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

driverInfo :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> Maybe (Id Common.Driver) -> Flow Common.DriverInfoRes
driverInfo merchantShortId opCity mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber mbEmail fleetOwnerId mbFleet mbPersonId = do
  when mbFleet $ do
    when (isNothing mbVehicleNumber) $ throwError $ InvalidRequest "Fleet Owner can only search with vehicle Number"
    vehicleInfo <- RCQuery.findLastVehicleRCFleet' (fromMaybe " " mbVehicleNumber) fleetOwnerId
    when (isNothing vehicleInfo) $ throwError $ InvalidRequest "Fleet Owner does not have a vehicle linked with this vehicle number"
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let mbPersonId' = cast @Common.Driver @DP.Person <$> mbPersonId
  driverWithRidesCount <- case (mbMobileNumber, mbVehicleNumber, mbDlNumber, mbRcNumber, mbEmail, mbPersonId') of
    (Just mobileNumber, Nothing, Nothing, Nothing, Nothing, Nothing) -> do
      mobileNumberDbHash <- getDbHash mobileNumber
      let mobileCountryCode = fromMaybe mobileIndianCode (appendPlusInMobileCountryCode mbMobileCountryCode)
      B.runInReplica $
        QDriverStats.fetchDriverInfoWithRidesCount merchant merchantOpCity (Just (mobileNumberDbHash, mobileCountryCode)) Nothing Nothing Nothing Nothing Nothing
          >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)
    (Nothing, Just vehicleNumber, Nothing, Nothing, Nothing, Nothing) -> do
      B.runInReplica $
        QDriverStats.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing (Just vehicleNumber) Nothing Nothing Nothing Nothing
          >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    (Nothing, Nothing, Just driverLicenseNumber, Nothing, Nothing, Nothing) -> do
      dlNumberHash <- getDbHash driverLicenseNumber
      B.runInReplica $
        QDriverStats.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing (Just dlNumberHash) Nothing Nothing Nothing
          >>= fromMaybeM (InvalidRequest "License does not exist.")
    (Nothing, Nothing, Nothing, Just rcNumber, Nothing, Nothing) -> do
      rcNumberHash <- getDbHash rcNumber
      B.runInReplica $
        QDriverStats.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing Nothing (Just rcNumberHash) Nothing Nothing
          >>= fromMaybeM (InvalidRequest "Registration certificate does not exist.")
    (Nothing, Nothing, Nothing, Nothing, Just email, Nothing) -> do
      B.runInReplica $
        QDriverStats.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing Nothing Nothing (Just email) Nothing
          >>= fromMaybeM (InvalidRequest "Email does not exist.")
    (Nothing, Nothing, Nothing, Nothing, Nothing, Just personId) -> do
      B.runInReplica $
        QDriverStats.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing Nothing Nothing Nothing (Just personId)
          >>= fromMaybeM (PersonDoesNotExist $ personId.getId)
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\", \"Email\" is required"
  let driverId = driverWithRidesCount.person.id
  mbDriverLicense <- B.runInReplica $ QDriverLicense.findByDriverId driverId
  rcAssociationHistory <- B.runInReplica $ QRCAssociation.findAllByDriverId driverId
  blockHistory <- B.runInReplica $ QDBT.findByDriverId driverId

  buildDriverInfoRes driverWithRidesCount mbDriverLicense rcAssociationHistory blockHistory

buildDriverInfoRes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  QPerson.DriverWithRidesCount ->
  Maybe DriverLicense ->
  [(DriverRCAssociation, VehicleRegistrationCertificate)] ->
  [DTDBT.DriverBlockTransactions] ->
  m Common.DriverInfoRes
buildDriverInfoRes QPerson.DriverWithRidesCount {..} mbDriverLicense rcAssociationHistory blockHistory = do
  mobileNumber <- traverse decrypt person.mobileNumber
  let email = person.email
  driverLicenseDetails <- traverse buildDriverLicenseAPIEntity mbDriverLicense
  let blockDetails = map buildBlockedListAPIEntity blockHistory
  vehicleRegistrationDetails <- traverse buildRCAssociationAPIEntity rcAssociationHistory
  unencryptedMobileNumber <- mapM decrypt person.mobileNumber
  unencryptedAlternateMobileNumber <- mapM decrypt person.alternateMobileNumber
  availableMerchants <- case unencryptedMobileNumber of
    Just mbNumber -> do
      let mobileCountryCode = fromMaybe "+91" person.mobileCountryCode
      mobileNumberHash <- getDbHash mbNumber
      availablePersonWithNumber <- QPerson.findAllMerchantIdByPhoneNo mobileCountryCode mobileNumberHash
      let availableMerchantsId = map (.merchantId) availablePersonWithNumber
      availableMerchantsShortId <- CQM.findAllShortIdById availableMerchantsId
      pure $ map getShortId availableMerchantsShortId
    Nothing -> pure []
  merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId
  driverStats <- runInReplica $ QDriverStats.findById person.id >>= fromMaybeM DriverInfoNotFound
  selectedServiceTiers <-
    maybe
      (pure [])
      ( \v ->
          v.selectedServiceTiers `forM` \serviceTierType -> do
            let mbServiceTier = find (\vst -> vst.serviceTierType == serviceTierType) cityVehicleServiceTiers
            return $ maybe (show serviceTierType) (.name) mbServiceTier
      )
      vehicle
  let serviceTierACThresholds =
        map
          (\DVST.VehicleServiceTier {..} -> airConditionedThreshold)
          (filter (\v -> maybe False (\veh -> veh.variant `elem` v.allowedVehicleVariant) vehicle) cityVehicleServiceTiers)
  let isACAllowedForDriver = checkIfACAllowedForDriver info (catMaybes serviceTierACThresholds)
  let isVehicleACWorking = maybe False (\v -> v.airConditioned /= Just False) vehicle
  cancellationData <- SCR.getCancellationRateData person.merchantOperatingCityId person.id

  pure
    Common.DriverInfoRes
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        numberOfRides = fromMaybe 0 ridesCount,
        mobileNumber,
        mobileCountryCode = person.mobileCountryCode,
        bundleVersion = person.clientBundleVersion,
        clientVersion = person.clientSdkVersion,
        enabled = info.enabled,
        blocked = info.blocked,
        blockedReason = info.blockedReason,
        verified = info.verified,
        subscribed = info.subscribed,
        onboardingDate = info.lastEnabledOn,
        canDowngradeToSedan = info.canDowngradeToSedan,
        canDowngradeToHatchback = info.canDowngradeToHatchback,
        canDowngradeToTaxi = info.canDowngradeToTaxi,
        canSwitchToRental = info.canSwitchToRental,
        canSwitchToInterCity = info.canSwitchToInterCity,
        vehicleNumber = vehicle <&> (.registrationNo),
        selectedServiceTiers,
        driverLicenseDetails,
        vehicleRegistrationDetails,
        rating = driverStats.rating,
        alternateNumber = unencryptedAlternateMobileNumber,
        availableMerchants = availableMerchants,
        merchantOperatingCity = merchantOperatingCity <&> (.city),
        blockStateModifier = info.blockStateModifier,
        currentAcOffReportCount = maybe 0 round info.airConditionScore,
        totalAcRestrictionUnblockCount = info.acRestrictionLiftCount,
        lastACStatusCheckedAt = info.lastACStatusCheckedAt,
        currentACStatus = isACAllowedForDriver && isVehicleACWorking,
        downgradeReason = vehicle >>= (.downgradeReason),
        assignedCount = (.assignedCount) <$> cancellationData,
        cancelledCount = (.cancelledCount) <$> cancellationData,
        cancellationRate = (.cancellationRate) <$> cancellationData,
        windowSize = (.windowSize) <$> cancellationData,
        blockedDueToRiderComplains = not isACAllowedForDriver,
        driverTag = person.driverTag,
        blockedInfo = blockDetails,
        email,
        softBlockStiers = info.softBlockStiers >>= (pure . map show),
        softBlockExpiryTime = info.softBlockExpiryTime,
        softBlockReasonFlag = info.softBlockReasonFlag
      }

buildDriverLicenseAPIEntity :: EncFlow m r => DriverLicense -> m Common.DriverLicenseAPIEntity
buildDriverLicenseAPIEntity DriverLicense {..} = do
  licenseNumber' <- decrypt licenseNumber
  pure
    Common.DriverLicenseAPIEntity
      { driverLicenseId = cast @DriverLicense @Common.DriverLicense id,
        documentImageId1 = cast @Image @Common.Image documentImageId1,
        documentImageId2 = (cast @Image @Common.Image) <$> documentImageId2,
        licenseNumber = licenseNumber',
        verificationStatus = castVerificationStatus verificationStatus,
        ..
      }

buildBlockedListAPIEntity :: DTDBT.DriverBlockTransactions -> Common.DriverBlockTransactions
buildBlockedListAPIEntity DTDBT.DriverBlockTransactions {..} =
  Common.DriverBlockTransactions
    { blockedBy = show blockedBy,
      blockReasonFlag = show <$> blockReasonFlag,
      ..
    }

buildRCAssociationAPIEntity ::
  EncFlow m r =>
  (DriverRCAssociation, VehicleRegistrationCertificate) ->
  m Common.DriverRCAssociationAPIEntity
buildRCAssociationAPIEntity (DriverRCAssociation {..}, vehicleRC) = do
  details <- buildVehicleRCAPIEntity vehicleRC
  pure Common.DriverRCAssociationAPIEntity {..}

buildVehicleRCAPIEntity :: EncFlow m r => VehicleRegistrationCertificate -> m Common.VehicleRegistrationCertificateAPIEntity
buildVehicleRCAPIEntity VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  pure
    Common.VehicleRegistrationCertificateAPIEntity
      { registrationCertificateId = cast @VehicleRegistrationCertificate @Common.VehicleRegistrationCertificate id,
        documentImageId = cast @Image @Common.Image documentImageId,
        certificateNumber = certificateNumber',
        verificationStatus = castVerificationStatus verificationStatus,
        vehicleVariant = castVehicleVariantDashboard vehicleVariant,
        ..
      }

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
unlinkVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
unlinkVehicle merchantShortId opCity reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  DomainRC.deactivateCurrentRC personId
  QVehicle.deleteById personId
  QDriverInfo.updateEnabledVerifiedState driverId False (Just False)
  logTagInfo "dashboard -> unlinkVehicle : " (show personId)
  pure Success

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
addVehicle :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.AddVehicleReq -> Flow APISuccess
addVehicle merchantShortId opCity reqDriverId req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM DriverInfoNotFound

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  mbLinkedVehicle <- QVehicle.findById personId
  whenJust mbLinkedVehicle $ \_ -> throwError VehicleAlreadyLinked

  allLinkedRCs <- QRCAssociation.findAllLinkedByDriverId personId
  unless (length allLinkedRCs < transporterConfig.rcLimit) $ throwError (RCLimitReached transporterConfig.rcLimit)

  let updDriver = driver {DP.firstName = req.driverName} :: DP.Person
  QPerson.updatePersonRec personId updDriver

  -- Create RC for vehicle before verifying it
  now <- getCurrentTime
  mbRC <- RCQuery.findLastVehicleRCWrapper req.registrationNo
  whenJust mbRC $ \rc -> do
    mbAssoc <- QRCAssociation.findLinkedByRCIdAndDriverId personId rc.id now
    when (isNothing mbAssoc) $ do
      driverRCAssoc <- makeRCAssociation merchant.id merchantOpCityId personId rc.id (convertTextToUTC (Just "2099-12-12"))
      QRCAssociation.create driverRCAssoc
    throwError $ InvalidRequest "RC already exists for this vehicle number, please activate."

  let createRCInput = createRCInputFromVehicle req
  mbNewRC <- buildRC merchant.id merchantOpCityId createRCInput
  case mbNewRC of
    Just newRC -> do
      when (newRC.verificationStatus == Documents.INVALID) $ do throwError (InvalidRequest $ "No valid mapping found for (vehicleClass: " <> req.vehicleClass <> ", manufacturer: " <> req.make <> " and model: " <> req.model <> ")")
      RCQuery.upsert newRC
      mbAssoc <- QRCAssociation.findLinkedByRCIdAndDriverId personId newRC.id now
      when (isNothing mbAssoc) $ do
        driverRCAssoc <- makeRCAssociation merchant.id merchantOpCityId personId newRC.id (convertTextToUTC (Just "2099-12-12"))
        QRCAssociation.create driverRCAssoc

      fork "Parallely verifying RC for add Vehicle: " $ runVerifyRCFlow personId merchant merchantOpCityId opCity req False -- run RC verification details
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
      driverInfo' <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
      let vehicle = makeFullVehicleFromRC cityVehicleServiceTiers driverInfo' driver merchant.id req.registrationNo newRC merchantOpCityId now
      QVehicle.create vehicle
      when (vehicle.variant == DV.SUV) $
        QDriverInfo.updateDriverDowngradeForSuv transporterConfig.canSuvDowngradeToHatchback transporterConfig.canSuvDowngradeToTaxi personId
      logTagInfo "dashboard -> addVehicle : " (show personId)
    Nothing -> throwError $ InvalidRequest "Registration Number is empty"
  pure Success

--------------------------------------------------------------------

data VehicleDetailsCSVRow = VehicleDetailsCSVRow
  { airConditioned :: Text,
    color :: Text,
    registrationNo :: Text,
    model :: Text,
    oxygen :: Text,
    ventilator :: Text,
    vehicleDoors :: Text,
    vehicleSeatBelts :: Text,
    vehicleManufacturer :: Text,
    dateOfRegistration :: Text
  }
  deriving (Show)

instance FromNamedRecord VehicleDetailsCSVRow where
  parseNamedRecord r =
    VehicleDetailsCSVRow
      <$> r .: "air_conditioned"
      <*> r .: "color"
      <*> r .: "registration_no"
      <*> r .: "model"
      <*> r .: "oxygen"
      <*> r .: "ventilator"
      <*> r .: "vehicle_doors"
      <*> r .: "vehicle_seat_belts"
      <*> r .: "vehicle_manufacturer"
      <*> r .: "date_of_registration"

instance FromMultipart Tmp Common.CreateVehiclesReq where
  fromMultipart form = do
    Common.CreateVehiclesReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp Common.CreateVehiclesReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

addVehiclesInFleet :: ShortId DM.Merchant -> Context.City -> Text -> Common.CreateVehiclesReq -> Flow APISuccess
addVehiclesInFleet merchantShortId opCity fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  rcReq <- readCsv req.file merchantOpCity
  when (length rcReq > 100) $ throwError $ InvalidRequest "Maximum 100 vehicles can be added in one go" -- TODO: Configure the limit
  mapM_ (registerRCForFleetWithoutDriver merchantShortId opCity fleetOwnerId) rcReq
  pure Success
  where
    readCsv csvFile merchantOpCity = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector VehicleDetailsCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM (parseVehicleInfo merchantOpCity) v >>= (pure . V.toList)

    parseVehicleInfo :: DMOC.MerchantOperatingCity -> Int -> VehicleDetailsCSVRow -> Flow Common.RegisterRCReq
    parseVehicleInfo moc idx row = do
      vehicleColour <- cleanCSVField idx row.color "Color"
      vehicleModel <- cleanCSVField idx row.model "Model"
      vehicleRegistrationCertNumber <- cleanCSVField idx row.registrationNo "Registration No"
      vehicleManufacturer <- cleanCSVField idx row.vehicleManufacturer "Vehicle Manufacturer"
      imageId <- generateGUID
      let dateOfRegistration :: Maybe UTCTime = readMaybeCSVField idx row.dateOfRegistration "Registration Date"
          operatingCity = show moc.city
          airConditioned :: Maybe Bool = readMaybeCSVField idx row.airConditioned "Air Conditioned"
          multipleRC = Just True
          oxygen :: Maybe Bool = readMaybeCSVField idx row.oxygen "Oxygen"
          ventilator :: Maybe Bool = readMaybeCSVField idx row.ventilator "Ventilator"
          vehicleSeatBelts :: Maybe Int = readMaybeCSVField idx row.vehicleSeatBelts "Seat Belts"
          vehicleDoors :: Maybe Int = readMaybeCSVField idx row.vehicleDoors "Vehicle Doors "
      pure Common.RegisterRCReq {vehicleDetails = Just Common.DriverVehicleDetails {..}, ..}

    cleanField = replaceEmpty . T.strip

    replaceEmpty :: Text -> Maybe Text
    replaceEmpty = \case
      "" -> Nothing
      "no constraint" -> Nothing
      "no_constraint" -> Nothing
      x -> Just x

    readMaybeCSVField :: Read a => Int -> Text -> Text -> Maybe a
    readMaybeCSVField _ fieldValue _ =
      cleanField fieldValue >>= readMaybe . T.unpack

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

---------------------------------------------------------------------

addVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Text -> Common.AddVehicleReq -> Flow APISuccess
addVehicleForFleet merchantShortId opCity reqDriverPhoneNo mbMobileCountryCode fleetOwnerId req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  phoneNumberHash <- getDbHash reqDriverPhoneNo
  let mobileCountryCode = fromMaybe mobileIndianCode mbMobileCountryCode
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
  void $ runVerifyRCFlow driver.id merchant merchantOpCityId opCity req True
  logTagInfo "dashboard -> addVehicle : " (show driver.id)
  pure Success

---------------------------------------------------------------------

registerRCForFleetWithoutDriver :: ShortId DM.Merchant -> Context.City -> Text -> Common.RegisterRCReq -> Flow APISuccess
registerRCForFleetWithoutDriver merchantShortId opCity fleetOwnerId req = do
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

createRCInputFromVehicle :: Common.AddVehicleReq -> CreateRCInput
createRCInputFromVehicle req@Common.AddVehicleReq {..} =
  CreateRCInput
    { registrationNumber = Just registrationNo,
      fitnessUpto = Nothing,
      fleetOwnerId = Nothing,
      vehicleCategory = Nothing,
      airConditioned,
      oxygen,
      ventilator,
      documentImageId = "",
      vehicleClass = Just vehicleClass,
      vehicleClassCategory = Nothing,
      insuranceValidity = Nothing,
      seatingCapacity = capacity,
      permitValidityUpto = Nothing,
      pucValidityUpto = Nothing,
      manufacturer = Just make,
      manufacturerModel = Just model,
      bodyType = Nothing,
      fuelType = energyType,
      mYManufacturing = mYManufacturing,
      color = Just colour,
      dateOfRegistration = req.dateOfRegistration,
      vehicleModelYear = req.vehicleModelYear
    }

checkRCAssociationForFleet :: Text -> VehicleRegistrationCertificate -> Flow ()
checkRCAssociationForFleet fleetOwnerId vehicleRC = do
  when (isJust vehicleRC.fleetOwnerId && vehicleRC.fleetOwnerId /= Just fleetOwnerId) $ throwError VehicleBelongsToAnotherFleet
  activeAssociationsOfRC <- DRCAE.findAllActiveAssociationByRCId vehicleRC.id
  let rcAssociatedDriverIds = map (.driverId) activeAssociationsOfRC
  forM_ rcAssociatedDriverIds $ \driverId -> do
    isFleetDriver <- FDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
    when (isNothing isFleetDriver) $ throwError (InvalidRequest "Vehicle is associated with a driver who is not part of this fleet, First Unlink the vehicle from that driver and then try again")

---------------------------------------------------------------------

setVehicleDriverRcStatusForFleet :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.RCStatusReq -> Flow APISuccess
setVehicleDriverRcStatusForFleet merchantShortId opCity reqDriverId fleetOwnerId req = do
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

getFleetDriverVehicleAssociation :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.DrivertoVehicleAssociationRes
getFleetDriverVehicleAssociation merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo = do
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
            let vehicleType = castVehicleVariantDashboard vrca.vehicleVariant
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

getFleetDriverAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Common.DriverMode ->
  Flow Common.DrivertoVehicleAssociationRes
getFleetDriverAssociation merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbDriverPhNo mbIsActive mbStats mbFrom mbTo mbMode = do
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
              let dlStatus = castVerificationStatus dl.verificationStatus
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
      let vehicleType = castVehicleVariantDashboard vrc.vehicleVariant
      pure (Just decryptedVehicleRC, vehicleType)

getFleetVehicleAssociation ::
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
getFleetVehicleAssociation merchantShortId _opCity fleetOwnerId mbLimit mbOffset mbVehicleNumber mbIncludeStats mbFrom mbTo mbStatus = do
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
        let vehicleType = castVehicleVariantDashboard vrc.vehicleVariant
        let isDriverActive = isJust driverName -- Check if there is a current active driver
        let isRcAssociated = isJust rcActiveAssociation
        let verificationDocs =
              Common.VerificationDocsStatus
                { vehicleRegistrationCertificate = Just $ castVerificationStatus vrc.verificationStatus,
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

castDriverStatus :: Maybe DrInfo.DriverMode -> Common.DriverMode
castDriverStatus = \case
  Just DrInfo.ONLINE -> Common.ONLINE
  Just DrInfo.OFFLINE -> Common.OFFLINE
  Just DrInfo.SILENT -> Common.SILENT
  Nothing -> Common.OFFLINE

castDashboardDriverStatus :: Common.DriverMode -> DrInfo.DriverMode
castDashboardDriverStatus = \case
  Common.ONLINE -> DrInfo.ONLINE
  Common.OFFLINE -> DrInfo.OFFLINE
  Common.SILENT -> DrInfo.SILENT

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

fleetUnlinkVehicle :: ShortId DM.Merchant -> Text -> Id Common.Driver -> Text -> Flow APISuccess
fleetUnlinkVehicle merchantShortId fleetOwnerId reqDriverId vehicleNo = do
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
  logTagInfo "fleet -> unlinkVehicle : " (show personId)
  pure Success

toggleDriverSubscriptionByService ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe (Id Plan) ->
  Bool ->
  Maybe Text ->
  Flow ()
toggleDriverSubscriptionByService (driverId, mId, mOpCityId) serviceName mbPlanToAssign toToggle mbVehicleNo = do
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
        notifyYatriRentalEventsToDriver mbVehicleNo WHATSAPP_VEHICLE_LINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
    else do
      when (isJust driverPlan) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName toToggle driverId serviceName
        fork "track service toggle" $ do
          case driverPlan of
            Just dp -> SEVT.trackServiceUsageChargeToggle dp Nothing
            Nothing -> pure ()
        fork "notify rental event" $ do
          notifyYatriRentalEventsToDriver mbVehicleNo WHATSAPP_VEHICLE_UNLINKED_MESSAGE driverId transporterConfig Nothing WHATSAPP
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
      let serviceSpecificData = maybe DDPlan.NoData DDPlan.RentedVehicleNumber mbVehicleNo
      _ <- DTPlan.planSubscribe serviceName planId (True, Just WHATSAPP) (cast driverId, mId, mOpCityId) driverInfo' serviceSpecificData
      pure ()

runVerifyRCFlow :: Id DP.Person -> DM.Merchant -> Id DMOC.MerchantOperatingCity -> Context.City -> Common.AddVehicleReq -> Bool -> Flow ()
runVerifyRCFlow personId merchant merchantOpCityId operatingCity req isFleet = do
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
            vehicleCategory = req.vehicleCategory
          }
  void $ DomainRC.verifyRC (not isFleet) (Just merchant) (personId, merchant.id, merchantOpCityId) rcReq

castVehicleVariantDashboard :: Maybe DV.VehicleVariant -> Maybe Common.VehicleVariant
castVehicleVariantDashboard = \case
  Just DV.SUV -> Just Common.SUV
  Just DV.HATCHBACK -> Just Common.HATCHBACK
  Just DV.SEDAN -> Just Common.SEDAN
  Just DV.AUTO_RICKSHAW -> Just Common.AUTO_RICKSHAW
  Just DV.TAXI -> Just Common.TAXI
  Just DV.TAXI_PLUS -> Just Common.TAXI_PLUS
  Just DV.PREMIUM_SEDAN -> Just Common.PREMIUM_SEDAN
  Just DV.BLACK -> Just Common.BLACK
  Just DV.BLACK_XL -> Just Common.BLACK_XL
  Just DV.BIKE -> Just Common.BIKE
  Just DV.AMBULANCE_TAXI -> Just Common.AMBULANCE_TAXI
  Just DV.AMBULANCE_TAXI_OXY -> Just Common.AMBULANCE_TAXI_OXY
  Just DV.AMBULANCE_AC -> Just Common.AMBULANCE_AC
  Just DV.AMBULANCE_AC_OXY -> Just Common.AMBULANCE_AC_OXY
  Just DV.AMBULANCE_VENTILATOR -> Just Common.AMBULANCE_VENTILATOR
  Just DV.SUV_PLUS -> Just Common.SUV_PLUS
  Just DV.DELIVERY_BIKE -> Just Common.DELIVERY_BIKE
  Just DV.DELIVERY_LIGHT_GOODS_VEHICLE -> Just Common.DELIVERY_LIGHT_GOODS_VEHICLE
  Just DV.BUS_NON_AC -> Just Common.BUS_NON_AC
  Just DV.BUS_AC -> Just Common.BUS_AC
  _ -> Nothing

---------------------------------------------------------------------
getAllVehicleForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Int -> Maybe Int -> Flow Common.ListVehicleRes
getAllVehicleForFleet _ _ fleetOwnerId mbLimit mbOffset = do
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
      { variant = castVehicleVariantDashboard vehicleVariant,
        model = vehicleModel,
        color = vehicleColor,
        registrationNo = certificateNumber'
      }

getAllDriverForFleet :: ShortId DM.Merchant -> Text -> Maybe Int -> Maybe Int -> Flow Common.FleetListDriverRes
getAllDriverForFleet _ fleetOwnerId mbLimit mbOffset = do
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
endRCAssociation :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
endRCAssociation merchantShortId opCity reqDriverId = do
  -- API should be deprecated
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId

  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  associations <- QRCAssociation.findAllLinkedByDriverId personId
  mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
  let mVehicleRC = listToMaybe (catMaybes mVehicleRCs)

  case mVehicleRC of
    Just vehicleRC -> do
      rcNo <- decrypt vehicleRC.certificateNumber
      void $ DomainRC.deleteRC (personId, merchant.id, merchantOpCityId) (DomainRC.DeleteRCReq {rcNo}) True
    Nothing -> throwError (InvalidRequest "No linked RC  to driver")

  QDriverInfo.updateEnabledVerifiedState driverId False (Just False)
  logTagInfo "dashboard -> endRCAssociation : " (show personId)
  pure Success

setRCStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RCStatusReq -> Flow APISuccess
setRCStatus merchantShortId opCity reqDriverId Common.RCStatusReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  DomainRC.linkRCStatus (personId, merchant.id, merchantOpCityId) (DomainRC.RCStatusReq {..})

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
  ServiceNames ->
  Id INV.Invoice ->
  Flow Driver.HistoryEntryDetailsEntityV2
getPaymentHistoryEntityDetails merchantShortId opCity driverId serviceName invoiceId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Driver.getHistoryEntryDetailsEntityV2 (personId, merchant.id, merchantOpCityId) invoiceId.getId serviceName

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
          void $ LF.rideDetails dI.ride.id SRide.CANCELLED merchant.id dI.ride.driverId dI.ride.fromLocation.lat dI.ride.fromLocation.lon Nothing
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

fleetRemoveVehicle :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow APISuccess
fleetRemoveVehicle _merchantShortId _ fleetOwnerId_ vehicleNo = do
  vehicle <- QVehicle.findByRegistrationNo vehicleNo
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
  pure Success
  where
    updatedVehicleRegistrationCertificate VehicleRegistrationCertificate {..} = VehicleRegistrationCertificate {fleetOwnerId = Nothing, ..}

fleetRemoveDriver :: ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow APISuccess
fleetRemoveDriver _merchantShortId _ fleetOwnerId driverId = do
  let personId = cast @Common.Driver @DP.Person driverId
  associationList <- QRCAssociation.findAllLinkedByDriverId personId
  forM_ associationList $ \assoc -> do
    rc <- RCQuery.findByRCIdAndFleetOwnerId assoc.rcId $ Just fleetOwnerId
    when (isJust rc) $ throwError (InvalidRequest "Driver is linked to fleet Vehicle , first unlink then try")
  FDV.endFleetDriverAssociation fleetOwnerId personId
  pure Success

fleetTotalEarning :: ShortId DM.Merchant -> Context.City -> Text -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetTotalEarningResponse -- TODO: This is thing should be in interval level this will become very slow when the data will grow
fleetTotalEarning _merchantShortId _ fleetOwnerId mbFrom mbTo = do
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

fleetVehicleEarning :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetEarningListRes
fleetVehicleEarning _merchantShortId _ fleetOwnerId mbVehicleNumber mbLimit mbOffset mbFrom mbTo = do
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
          vehicleType = castVehicleVariantDashboard rc.vehicleVariant,
          totalDuration = totalDuration,
          distanceTravelled = fromIntegral distanceTravelled / 1000.0,
          driverPhoneNo = Nothing,
          cancelledRides = cancelledRides
        }
  let summary = Common.Summary {totalCount = 10000, count = length res}
  pure $ Common.FleetEarningListRes {fleetEarningRes = res, summary}

fleetDriverEarning :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Common.SortOn -> Flow Common.FleetEarningListRes
fleetDriverEarning merchantShortId _ fleetOwnerId mbMobileCountryCode mbDriverPhNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn = do
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

------------------------------------------------------------------------------------------------

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
  case (driverPlan, req.planId) of
    (Just dp, Just planId) -> do
      if dp.planId == Id planId
        then do
          void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId
        else do
          void $ DTPlan.planSwitch serviceName (Id planId) (driver.id, driver.merchantId, driver.merchantOperatingCityId)
          void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId
    (Nothing, Just _) -> do
      void $ toggleDriverSubscriptionByService (driver.id, driver.merchantId, driver.merchantOperatingCityId) serviceName (Id <$> req.planId) req.serviceChargeEligibility req.vehicleId
    (Just dp, Nothing) -> do
      transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let enableServiceUsageCharge = dp.enableServiceUsageCharge
      when (enableServiceUsageCharge /= req.serviceChargeEligibility) $ do
        QDP.updateEnableServiceUsageChargeByDriverIdAndServiceName req.serviceChargeEligibility personId serviceName
        fork "track service toggle" $ do
          SEVT.trackServiceUsageChargeToggle dp (show <$> req.reason)
        when (serviceName == YATRI_RENTAL) $ do
          fork "notify rental event" $ do
            notifyYatriRentalEventsToDriver req.vehicleId (getMkeyForEvent req.serviceChargeEligibility) personId transporterConfig (show <$> req.reason) WHATSAPP
    (Nothing, Nothing) -> throwError $ InvalidRequest "pls provide a plan Id to enable subscription"
  pure Success
  where
    getMkeyForEvent serviceChargeEligiblity = if serviceChargeEligiblity then YATRI_RENTAL_RESUME else YATRI_RENTAL_PAUSE

notifyYatriRentalEventsToDriver :: Maybe Text -> MessageKey -> Id DP.Person -> TransporterConfig -> Maybe Text -> MediaChannel -> Flow ()
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
        (mbSender, message) <- MessageBuilder.buildGenericMessage merchantOpCityId mkey Nothing MessageBuilder.BuildGenericMessageReq {}
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
      WHATSAPP -> do
        merchantMessage <-
          QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId mkey Nothing
            >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
        result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOpCityId (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId (Just $ fromMaybe "XXXXX" vehicleId) (Just timeStamp) mbReason Nothing (Just merchantMessage.containsUrlButton))
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
      _ -> pure ()

updateRCInvalidStatus :: ShortId DM.Merchant -> Context.City -> Common.UpdateRCInvalidStatusReq -> Flow APISuccess
updateRCInvalidStatus _ _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  RCQuery.updateVehicleVariant vehicleRC.id (Just req.vehicleVariant) Nothing (Just True)
  pure Success

updateVehicleVariant :: ShortId DM.Merchant -> Context.City -> Common.UpdateVehicleVariantReq -> Flow APISuccess
updateVehicleVariant _ _ req = do
  vehicleRC <- RCQuery.findById (Id req.rcId) >>= fromMaybeM (VehicleNotFound req.rcId)
  rcNumber <- decrypt vehicleRC.certificateNumber
  mVehicle <- QVehicle.findByRegistrationNo rcNumber
  RCQuery.updateVehicleVariant vehicleRC.id (Just req.vehicleVariant) Nothing Nothing
  whenJust mVehicle $ \vehicle -> updateVehicleVariantAndServiceTier req.vehicleVariant vehicle $ DV.castVehicleVariantToVehicleCategory req.vehicleVariant
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
      rcNumber <- decrypt vehicleRC.certificateNumber
      mVehicle <- QVehicle.findByRegistrationNo rcNumber
      RCQuery.updateVehicleVariant vehicleRC.id rcReq.vehicleVariant rcReq.markReviewed (not <$> rcReq.markReviewed)
      whenJust mVehicle $ \vehicle -> do
        whenJust rcReq.vehicleVariant $ \variant -> updateVehicleVariantAndServiceTier variant vehicle $ DV.castVehicleVariantToVehicleCategory variant

updateVehicleVariantAndServiceTier :: DV.VehicleVariant -> DVeh.Vehicle -> DVC.VehicleCategory -> Flow ()
updateVehicleVariantAndServiceTier variant vehicle vehicleCategory = do
  driver <- B.runInReplica $ QPerson.findById vehicle.driverId >>= fromMaybeM (PersonDoesNotExist vehicle.driverId.getId)
  driverInfo' <- QDriverInfo.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  -- driverStats <- runInReplica $ QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
  vehicleServiceTiers <- CQVST.findAllByMerchantOpCityId driver.merchantOperatingCityId
  let availableServiceTiersForDriver = (.serviceTierType) . fst <$> selectVehicleTierForDriverWithUsageRestriction True driverInfo' vehicle vehicleServiceTiers
  QVehicle.updateVariantAndServiceTiers variant availableServiceTiersForDriver (Just vehicleCategory) vehicle.driverId

updateDriverTag :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverTagReq -> Flow APISuccess
updateDriverTag merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  when (req.isAddingTag && maybe False (elem req.driverTag) driver.driverTag) $ throwError (InvalidRequest "Tag already exists")
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  Yudhishthira.verifyTag req.driverTag
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

updateFleetOwnerInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateFleetOwnerInfoReq -> Flow APISuccess
updateFleetOwnerInfo merchantShortId opCity driverId req = do
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

getFleetOwnerInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.FleetOwnerInfoRes
getFleetOwnerInfo _ _ driverId = do
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

linkRCWithDriverForFleet :: ShortId DM.Merchant -> Context.City -> Text -> Common.LinkRCWithDriverForFleetReq -> Flow APISuccess
linkRCWithDriverForFleet merchantShortId opCity fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  phoneNumberHash <- getDbHash req.driverMobileNumber
  let mobileCountryCode = fromMaybe mobileIndianCode req.driverMobileCountryCode
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

getDriverPersonNumbers :: ShortId DM.Merchant -> Context.City -> Common.PersonIdsReq -> Flow [Common.PersonRes]
getDriverPersonNumbers _ _ req = do
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

getDriverPersonId :: ShortId DM.Merchant -> Context.City -> Common.PersonMobileNoReq -> Flow [Common.PersonRes]
getDriverPersonId _ _ req = do
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

postDriverUpdateVehicleManufacturing :: (ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateVehicleManufacturingReq -> Flow APISuccess)
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

mapFeeType :: Common.DriverFeeType -> DDF.FeeType
mapFeeType Common.PAYOUT_REGISTRATION = DDF.PAYOUT_REGISTRATION
mapFeeType Common.ONE_TIME_SECURITY_DEPOSIT = DDF.ONE_TIME_SECURITY_DEPOSIT

postDriverRefundByPayout :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RefundByPayoutReq -> Flow APISuccess
postDriverRefundByPayout merchantShortId _opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let refundByPayoutReq =
        DDriver.RefundByPayoutReq
          { serviceName = mapServiceName req.serviceName,
            refundAmountDeduction = req.refundAmountDeduction,
            payerVpa = req.payerVpa,
            driverFeeType = mapFeeType req.driverFeeType,
            refundAmountSegregation = req.refundAmountSegregation
          }
  void $ DDriver.refundByPayoutDriverFee (personId, driver.merchantId, merchantOpCityId) refundByPayoutReq
  return Success

getDriverSecurityDepositStatus :: (ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Common.ServiceNames -> Flow [Common.SecurityDepositDfStatusRes])
getDriverSecurityDepositStatus merchantShortId _opCity driverId serviceName' = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  let serviceName = maybe YATRI_RENTAL mapServiceName serviceName'
  response <- DDriver.getSecurityDepositDfStatus (personId, driver.merchantId, merchantOpCityId) serviceName
  return $ mapSecurityDepositDfStatusResToDashboardType response
  where
    mapSecurityDepositDfStatusResToDashboardType =
      map
        ( \(DDriver.SecurityDepositDfStatusRes {..}) -> do
            Common.SecurityDepositDfStatusRes {securityDepositStatus = castStatus securityDepositStatus, ..}
        )

getDriverPanAadharSelfieDetailsList :: (ShortId DM.Merchant -> Context.City -> Text -> Id Common.Driver -> Flow [Common.PanAadharSelfieDetailsListResp])
getDriverPanAadharSelfieDetailsList merchantShortId _opCity docType' driverID = do
  let personId = cast @Common.Driver @DP.Person driverID
  merchant <- findMerchantByShortId merchantShortId
  documentType <- convertToDomainType docType'
  hvSdkLogs <- QSdkLogs.findAllByDriverIdAndDocType personId $ Just documentType
  imageDetails <- filter (isJust . (.workflowTransactionId)) <$> QImage.findImagesByPersonAndType merchant.id personId documentType
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
