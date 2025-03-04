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
    postDriverExemptDriverFee,
  )
where

import qualified "this" API.Types.Dashboard.RideBooking.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import Domain.Types.DriverFee as DDF
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
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverBlockTransactions as QDBT
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
  Flow [Common.DriverOutstandingBalanceResp] -- add mig and totalFee
getDriverPaymentDue merchantShortId _ mbMobileCountryCode phone = do
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
          status_ = DCommon.castStatus status
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
postDriverCollectCash mId city driver requestorId = recordPayment False mId city driver requestorId YATRI_SUBSCRIPTION Nothing

---------------------------------------------------------------------
postDriverV2CollectCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Flow APISuccess
postDriverV2CollectCash mId city driver requestorId serviceName = collectCashV2 mId city driver requestorId serviceName Nothing

collectCashV2 :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.ServiceNames -> Maybe Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
collectCashV2 mId city driver requestorId serviceName mbExemptionAndCashCollectionDriverFeeReq = recordPayment False mId city driver requestorId (DCommon.mapServiceName serviceName) mbExemptionAndCashCollectionDriverFeeReq

---------------------------------------------------------------------
postDriverExemptCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverExemptCash mId city driver requestorId = recordPayment True mId city driver requestorId YATRI_SUBSCRIPTION Nothing

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
postDriverV2ExemptCash ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Flow APISuccess
postDriverV2ExemptCash mId city driver requestorId serviceName = exemptCashV2 mId city driver requestorId serviceName Nothing

exemptCashV2 :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.ServiceNames -> Maybe Common.ExemptionAndCashCollectionDriverFeeReq -> Flow APISuccess
exemptCashV2 mId city driver requestorId serviceName mbExemptionAndCashCollectionDriverFeeReq = recordPayment True mId city driver requestorId (DCommon.mapServiceName serviceName) mbExemptionAndCashCollectionDriverFeeReq

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
        canSwitchToIntraCity = info.canSwitchToIntraCity,
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
        driverTag = fmap Yudhishthira.removeTagExpiry <$> person.driverTag,
        driverTagObject = fmap Yudhishthira.convertToTagObject <$> person.driverTag,
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
        vehicleVariant = DCommon.castVehicleVariantDashboard vehicleVariant,
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
      vehicleModelYear = req.vehicleModelYear,
      grossVehicleWeight = Nothing,
      unladdenWeight = Nothing
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

---------------------------------------------------------------------
postDriverExemptDriverFee ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.ServiceNames ->
  Common.ExemptionAndCashCollectionDriverFeeReq ->
  Flow APISuccess
postDriverExemptDriverFee mId city driver requestorId serviceName req = do
  if req.isExempt
    then exemptCashV2 mId city driver requestorId serviceName (Just req)
    else collectCashV2 mId city driver requestorId serviceName (Just req)
