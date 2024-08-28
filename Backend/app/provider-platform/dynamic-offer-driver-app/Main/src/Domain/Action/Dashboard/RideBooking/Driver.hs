{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideBooking.Driver
  ( getDriverPaymentDue,
    postDriverV2CollectCash,
    postDriverCollectCash,
    postDriverV2ExemptCash,
    postDriverExemptCash,
    postDriverEnable,
    getDriverInfo,
    postDriverUnlinkVehicle,
    postDriverEndRCAssociation,
    postDriverSetRCStatus,
    postDriverAddVehicle,
  )
where

import qualified Dashboard.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.RideBooking.Driver as Common
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.DriverFee
import Domain.Types.DriverLicense
import Domain.Types.DriverRCAssociation
import Domain.Types.Image (Image)
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.Queries.DriverFee (findPendingFeesByDriverIdAndServiceName)
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error
import qualified Tools.SMS as Sms
import Utils.Common.Cac.KeyNameConstants

getDriverPaymentDue ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Text ->
  Flow [Common.DriverOutstandingBalanceResp]
getDriverPaymentDue merchantShortId _ mbMobileCountryCode phone = do
  let mobileCountryCode = fromMaybe "+91" mbMobileCountryCode
  merchant <- findMerchantByShortId merchantShortId
  mobileNumber <- getDbHash phone
  driver <- B.runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumber merchant.id DP.DRIVER >>= fromMaybeM (InvalidRequest "Person not found")
  driverFees <- findPendingFeesByDriverIdAndServiceName (cast driver.id) YATRI_SUBSCRIPTION
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

---------------------------------------------------------------------
postDriverEnable ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverEnable merchantShortId opCity reqDriverId = do
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

  when (isNothing mVehicle && null linkedRCs) $
    throwError (InvalidRequest "Can't enable driver if no vehicle or no RCs are linked to them")

  enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId driverId False
  logTagInfo "dashboard -> enableDriver : " (show personId)
  fork "sending dashboard sms - onboarding" $ do
    Sms.sendDashboardSms merchant.id merchantOpCityId Sms.ONBOARDING Nothing personId Nothing 0
  pure Success

---------------------------------------------------------------------
postDriverCollectCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverCollectCash mId city driver requestorId = recordPayment False mId city driver requestorId YATRI_SUBSCRIPTION

recordPaymentLockKey :: Id Common.Driver -> Text
recordPaymentLockKey driverId = "RP:LK:DId:-" <> driverId.getId

recordPayment :: Bool -> ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> ServiceNames -> Flow APISuccess
recordPayment isExempted merchantShortId opCity reqDriverId requestorId serviceName = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica (QPerson.findById personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)

  Redis.whenWithLockRedis (recordPaymentLockKey reqDriverId) 30 $ do
    -- merchant access checking
    let merchantId = driver.merchantId
    unless (merchant.id == merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
    driverFees <- findPendingFeesByDriverIdAndServiceName driverId serviceName
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
            merchantOperatingCityId = driverFee.merchantOperatingCityId,
            updatedAt = now,
            createdAt = now
          }

paymentStatus :: Bool -> DriverFeeStatus
paymentStatus isExempted
  | isExempted = EXEMPTED
  | otherwise = COLLECTED_CASH

---------------------------------------------------------------------
postDriverV2CollectCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Flow APISuccess
postDriverV2CollectCash mId city driver requestorId serviceName = recordPayment False mId city driver requestorId (DCommon.mapServiceName serviceName)

---------------------------------------------------------------------
postDriverExemptCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverExemptCash mId city driver requestorId = recordPayment True mId city driver requestorId YATRI_SUBSCRIPTION

---------------------------------------------------------------------
postDriverV2ExemptCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Flow APISuccess
postDriverV2ExemptCash mId city driver requestorId serviceName = recordPayment True mId city driver requestorId (DCommon.mapServiceName serviceName)

---------------------------------------------------------------------
getDriverInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Bool ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id Common.Driver) ->
  Flow Common.DriverInfoRes
getDriverInfo merchantShortId opCity fleetOwnerId mbFleet mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber mbEmail mbPersonId = do
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
      let mobileCountryCode = fromMaybe DCommon.mobileIndianCode (DCommon.appendPlusInMobileCountryCode mbMobileCountryCode)
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant merchantOpCity (Just (mobileNumberDbHash, mobileCountryCode)) Nothing Nothing Nothing Nothing Nothing
          >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)
    (Nothing, Just vehicleNumber, Nothing, Nothing, Nothing, Nothing) -> do
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing (Just vehicleNumber) Nothing Nothing Nothing Nothing
          >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    (Nothing, Nothing, Just driverLicenseNumber, Nothing, Nothing, Nothing) -> do
      dlNumberHash <- getDbHash driverLicenseNumber
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing (Just dlNumberHash) Nothing Nothing Nothing
          >>= fromMaybeM (InvalidRequest "License does not exist.")
    (Nothing, Nothing, Nothing, Just rcNumber, Nothing, Nothing) -> do
      rcNumberHash <- getDbHash rcNumber
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing Nothing (Just rcNumberHash) Nothing Nothing
          >>= fromMaybeM (InvalidRequest "Registration certificate does not exist.")
    (Nothing, Nothing, Nothing, Nothing, Just email, Nothing) -> do
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing Nothing Nothing (Just email) Nothing
          >>= fromMaybeM (InvalidRequest "Email does not exist.")
    (Nothing, Nothing, Nothing, Nothing, Nothing, Just personId) -> do
      B.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant merchantOpCity Nothing Nothing Nothing Nothing Nothing (Just personId)
          >>= fromMaybeM (PersonDoesNotExist $ personId.getId)
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\", \"Email\" is required"
  let driverId = driverWithRidesCount.person.id
  mbDriverLicense <- B.runInReplica $ QDriverLicense.findByDriverId driverId
  rcAssociationHistory <- B.runInReplica $ QRCAssociation.findAllByDriverId driverId
  buildDriverInfoRes driverWithRidesCount mbDriverLicense rcAssociationHistory

buildDriverInfoRes ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  QPerson.DriverWithRidesCount ->
  Maybe DriverLicense ->
  [(DriverRCAssociation, VehicleRegistrationCertificate)] ->
  m Common.DriverInfoRes
buildDriverInfoRes QPerson.DriverWithRidesCount {..} mbDriverLicense rcAssociationHistory = do
  mobileNumber <- traverse decrypt person.mobileNumber
  let email = person.email
  driverLicenseDetails <- traverse buildDriverLicenseAPIEntity mbDriverLicense
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
        blockedDueToRiderComplains = not isACAllowedForDriver,
        driverTag = person.driverTag,
        email
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
        verificationStatus = DCommon.castVerificationStatus verificationStatus,
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
        verificationStatus = DCommon.castVerificationStatus verificationStatus,
        vehicleVariant = DCommon.castVehicleVariantDashboard vehicleVariant,
        ..
      }

---------------------------------------------------------------------
postDriverUnlinkVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverUnlinkVehicle merchantShortId opCity reqDriverId = do
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
postDriverEndRCAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverEndRCAssociation merchantShortId opCity reqDriverId = do
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

---------------------------------------------------------------------
postDriverAddVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverAddVehicle merchantShortId opCity reqDriverId req = do
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

      fork "Parallely verifying RC for add Vehicle: " $ DCommon.runVerifyRCFlow personId merchant merchantOpCityId opCity req False -- run RC verification details
      cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId
      driverInfo' <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
      let vehicle = makeFullVehicleFromRC cityVehicleServiceTiers driverInfo' driver merchant.id req.registrationNo newRC merchantOpCityId now
      QVehicle.create vehicle
      when (vehicle.variant == DV.SUV) $
        QDriverInfo.updateDriverDowngradeForSuv transporterConfig.canSuvDowngradeToHatchback transporterConfig.canSuvDowngradeToTaxi personId
      logTagInfo "dashboard -> addVehicle : " (show personId)
    Nothing -> throwError $ InvalidRequest "Registration Number is empty"
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

---------------------------------------------------------------------
postDriverSetRCStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.RCStatusReq ->
  Flow APISuccess
postDriverSetRCStatus merchantShortId opCity reqDriverId Common.RCStatusReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId && merchantOpCityId == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)
  DomainRC.linkRCStatus (personId, merchant.id, merchantOpCityId) (DomainRC.RCStatusReq {..})
