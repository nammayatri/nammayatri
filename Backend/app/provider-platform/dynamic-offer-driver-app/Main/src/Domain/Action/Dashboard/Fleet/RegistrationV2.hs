module Domain.Action.Dashboard.Fleet.RegistrationV2
  ( postRegistrationV2LoginOtp,
    postRegistrationV2VerifyOtp,
    postRegistrationV2Register,
    castFleetTypeToDomain,
    buildFleetOwnerAuthReq,
    createFleetOwnerDetails,
    createFleetOwnerInfo,
    fleetOwnerLogin,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2 as Common
import Domain.Action.Dashboard.Fleet.Referral
import qualified Domain.Action.Dashboard.Fleet.Registration as DRegistration
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverReferral as DR
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.DocumentVerificationConfig as DVC
import Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding as DomainRC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverGstin as QGST
import qualified Storage.Queries.DriverPanCard as QPanCard
import Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.SMS as Sms hiding (Success)

postRegistrationV2LoginOtp ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Bool ->
  Common.FleetOwnerLoginReqV2 ->
  Flow Common.FleetOwnerLoginResV2
postRegistrationV2LoginOtp merchantShortId opCity enabled req = do
  fleetOwnerLogin merchantShortId opCity Nothing (Just enabled) req

postRegistrationV2VerifyOtp ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Common.FleetOwnerVerifyReqV2 ->
  Flow Kernel.Types.APISuccess.APISuccess
postRegistrationV2VerifyOtp merchantShortId opCity req = do
  fleetOwnerVerify merchantShortId opCity req

postRegistrationV2Register ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Text ->
  Common.FleetOwnerRegisterReqV2 ->
  Flow Common.FleetOwnerRegisterResV2
postRegistrationV2Register merchantShortId opCity requestorId req =
  fleetOwnerRegister merchantShortId opCity (Just requestorId) req

-- TODO check merchant and requestor access
fleetOwnerRegister ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Maybe Text ->
  Common.FleetOwnerRegisterReqV2 ->
  Flow Common.FleetOwnerRegisterResV2
fleetOwnerRegister _merchantShortId _opCity mbRequestorId req = do
  runRequestValidation Common.validateRegisterReqV2 req
  let mbPersonId = cast @Common.Person @DP.Person <$> req.personId
  fleetOwnerId <- (mbPersonId <|> (Id <$> mbRequestorId)) & fromMaybeM (InvalidRequest "personId required")

  person <- QP.findById fleetOwnerId >>= fromMaybeM (PersonDoesNotExist fleetOwnerId.getId)
  unless (person.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "personId should be fleet owner")
  fleetOwnerInfo <- QFOI.findByPrimaryKey fleetOwnerId >>= fromMaybeM (PersonDoesNotExist fleetOwnerId.getId)
  -- TODO: Refactor later --
  case fleetOwnerInfo.panImageId of
    Nothing -> throwError $ InvalidRequest "PAN not uploaded"
    Just imageId -> do
      panCard <- QPanCard.findByImageId (Id imageId) >>= fromMaybeM (InvalidRequest ("PAN not uploaded " <> imageId))
      unless (panCard.verificationStatus == Documents.VALID) $ throwError $ InvalidRequest "PAN not validated"
  case fleetOwnerInfo.gstImageId of
    Nothing -> throwError $ InvalidRequest "GST not uploaded"
    Just imageId -> do
      gstIn <- QGST.findByImageId (Id imageId) >>= fromMaybeM (InvalidRequest ("GST not uploaded " <> imageId))
      unless (gstIn.verificationStatus == Documents.VALID) $ throwError $ InvalidRequest "GST not validated"
  case fleetOwnerInfo.aadhaarFrontImageId of
    Nothing -> throwError $ InvalidRequest "Aadhaar front image not uploaded"
    Just imageId -> do
      aadhaarCard <- QAadhaarCard.findByFrontImageId (Just $ Id imageId) >>= fromMaybeM (InvalidRequest ("Aadhaar front image not uploaded " <> imageId))
      unless (aadhaarCard.verificationStatus == Documents.VALID) $ throwError $ InvalidRequest "Aadhaar front image not validated"
  case fleetOwnerInfo.aadhaarBackImageId of
    Nothing -> throwError $ InvalidRequest "Aadhaar back image not uploaded"
    Just imageId -> do
      aadhaarCard <- QAadhaarCard.findByBackImageId (Just $ Id imageId) >>= fromMaybeM (InvalidRequest ("Aadhaar back image not uploaded " <> imageId))
      unless (aadhaarCard.verificationStatus == Documents.VALID) $ throwError $ InvalidRequest "Aadhaar back image not validated"

  mbRequestedOperatorId <- case mbRequestorId of
    Just requestorId -> do
      requestor <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
      when (requestor.role == DP.FLEET_OWNER) $
        unless (requestor.id == fleetOwnerId) $ throwError AccessDenied
      if (requestor.role == DP.OPERATOR) then pure (Just requestor.id.getId) else pure Nothing
    Nothing -> pure Nothing

  whenJust req.email $ \reqEmail -> do
    unless (req.email == person.email) $
      unlessM (isNothing <$> QP.findByEmailAndMerchantIdAndRole (Just reqEmail) person.merchantId DP.FLEET_OWNER) $
        throwError (EmailAlreadyLinked reqEmail)

  let updPerson = person{firstName = req.firstName, lastName = Just req.lastName, email = req.email <|> person.email}
  void $ QP.updateByPrimaryKey updPerson
  void $ updateFleetOwnerInfo fleetOwnerInfo req
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)

  mbReferredOperatorId <- getOperatorIdFromReferralCode req.operatorReferralCode
  whenJust (mbReferredOperatorId <|> mbRequestedOperatorId) $ \referredOperatorId -> do
    fleetAssocs <- QFOA.findAllFleetAssociations fleetOwnerId.getId
    when (null fleetAssocs) $ do
      fleetOperatorAssocData <- SA.makeFleetOperatorAssociation person.merchantId person.merchantOperatingCityId fleetOwnerId.getId referredOperatorId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QFOA.create fleetOperatorAssocData
      DOR.incrementOnboardedCount DOR.FleetReferral (Id referredOperatorId) transporterConfig
  when (transporterConfig.generateReferralCodeForFleet == Just True) $ do
    fleetReferral <- QDR.findById person.id
    when (isNothing fleetReferral) $ void $ DR.generateReferralCode (Just DP.FLEET_OWNER) (fleetOwnerId, person.merchantId, person.merchantOperatingCityId)
  fork "Uploading Business License Image" $ do
    whenJust req.businessLicenseImage $ \businessLicenseImage -> do
      let req' = Image.ImageValidateRequest {imageType = DVC.BusinessLicense, image = businessLicenseImage, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
      image <- Image.validateImage True (fleetOwnerId, person.merchantId, person.merchantOperatingCityId) req'
      businessLicenseNumber <- forM req.businessLicenseNumber encrypt
      QFOI.updateBusinessLicenseImageAndNumber (Just image.imageId.getId) businessLicenseNumber fleetOwnerId
  enabled <- enableFleetIfPossible fleetOwnerId
  return $ Common.FleetOwnerRegisterResV2 enabled
  where
    enableFleetIfPossible :: Id DP.Person -> Flow Bool
    enableFleetIfPossible fleetOwnerId = do
      if (req.adminApprovalRequired /= Just True)
        then do
          panCard <- QPanCard.findByDriverId fleetOwnerId
          gstIn <- QGST.findByDriverId fleetOwnerId
          case castFleetType <$> req.fleetType of
            Just FOI.NORMAL_FLEET ->
              case panCard of
                Just pan | pan.verificationStatus == Documents.VALID -> do
                  void $ QFOI.updateFleetOwnerEnabledStatus True fleetOwnerId
                  pure True
                _ -> pure False
            Just FOI.BUSINESS_FLEET ->
              case (panCard, gstIn) of
                (Just pan, Just gst)
                  | pan.verificationStatus == Documents.VALID
                      && gst.verificationStatus == Documents.VALID -> do
                    void $ QFOI.updateFleetOwnerEnabledStatus True fleetOwnerId
                    pure True
                _ -> pure False
            _ -> pure False
        else pure False

castFleetType :: Common.FleetType -> FOI.FleetType
castFleetType = \case
  Common.RENTAL_FLEET -> FOI.RENTAL_FLEET
  Common.NORMAL_FLEET -> FOI.NORMAL_FLEET
  Common.BUSINESS_FLEET -> FOI.BUSINESS_FLEET

castFleetTypeToDomain :: FOI.FleetType -> Common.FleetType
castFleetTypeToDomain = \case
  FOI.RENTAL_FLEET -> Common.RENTAL_FLEET
  FOI.NORMAL_FLEET -> Common.NORMAL_FLEET
  FOI.BUSINESS_FLEET -> Common.BUSINESS_FLEET

getOperatorIdFromReferralCode :: Maybe Text -> Flow (Maybe Text)
getOperatorIdFromReferralCode Nothing = return Nothing
getOperatorIdFromReferralCode (Just refCode) = do
  let referralReq = FleetReferralReq {value = refCode}
  result <- isValidReferralForRole referralReq DP.OPERATOR
  case result of
    SuccessCode val -> return $ Just val

createFleetOwnerDetails :: Registration.AuthReq -> Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Maybe Bool -> Flow DP.Person
createFleetOwnerDetails authReq merchantId merchantOpCityId isDashboard deploymentVersion enabled = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- Registration.makePerson authReq transporterConfig Nothing Nothing Nothing Nothing (Just deploymentVersion) merchantId merchantOpCityId isDashboard (Just DP.FLEET_OWNER)
  void $ QP.create person
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  QDriverStats.createInitialDriverStats merchantOperatingCity.currency merchantOperatingCity.distanceUnit person.id
  fork "creating fleet owner info" $ createFleetOwnerInfo person.id merchantId enabled
  pure person

createFleetOwnerInfo :: Id DP.Person -> Id DMerchant.Merchant -> Maybe Bool -> Flow ()
createFleetOwnerInfo personId merchantId enabled = do
  now <- getCurrentTime
  let fleetOwnerInfo =
        FOI.FleetOwnerInformation
          { fleetOwnerPersonId = personId,
            merchantId = merchantId,
            fleetType = NORMAL_FLEET, -- overwrite in register
            enabled = fromMaybe True enabled,
            blocked = False,
            verified = False,
            gstNumber = Nothing,
            gstNumberDec = Nothing,
            gstImageId = Nothing,
            businessLicenseImageId = Nothing,
            businessLicenseNumber = Nothing,
            businessLicenseNumberDec = Nothing,
            referredByOperatorId = Nothing,
            aadhaarBackImageId = Nothing,
            aadhaarFrontImageId = Nothing,
            aadhaarNumber = Nothing,
            aadhaarNumberDec = Nothing,
            panImageId = Nothing,
            panNumber = Nothing,
            panNumberDec = Nothing,
            createdAt = now,
            updatedAt = now,
            registeredAt = Nothing,
            isEligibleForSubscription = True
          }
  QFOI.create fleetOwnerInfo

fleetOwnerLogin ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Maybe Text ->
  Maybe Bool ->
  Common.FleetOwnerLoginReqV2 ->
  Flow Common.FleetOwnerLoginResV2
fleetOwnerLogin merchantShortId opCity _mbRequestorId enabled req = do
  runRequestValidation Common.validateInitiateLoginReqV2 req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mobileNumberHash <- getDbHash mobileNumber
  mbPerson <- QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR]
  personId <- case mbPerson of
    Just person -> pure person.id
    Nothing -> do
      -- Operator won't reach here as it has separate sign up flow --
      let personAuth = buildFleetOwnerAuthReq merchant.id opCity req
      deploymentVersion <- asks (.version)
      person <- createFleetOwnerDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion enabled
      pure person.id
  let useFakeOtpM = useFakeSms smsCfg
  otp <- maybe generateOTPCode (return . show) useFakeOtpM
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let otpCode = otp
        phoneNumber = countryCode <> mobileNumber
    withLogTag ("mobileNumber" <> req.mobileNumber) $
      do
        (mbSender, message) <-
          MessageBuilder.buildSendOTPMessage merchantOpCityId $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  let key = makeMobileNumberOtpKey mobileNumber
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  void $ Redis.setExp key otp expTime
  pure $ Common.FleetOwnerLoginResV2 {personId = cast @DP.Person @Common.Person personId}

buildFleetOwnerAuthReq ::
  Id DMerchant.Merchant ->
  City.City ->
  Common.FleetOwnerLoginReqV2 ->
  Registration.AuthReq
buildFleetOwnerAuthReq merchantId' opCity Common.FleetOwnerLoginReqV2 {..} =
  Registration.AuthReq
    { name = Just "Fleet Owner", -- to be updated in register
      mobileNumber = Just mobileNumber,
      mobileCountryCode = Just mobileCountryCode,
      merchantId = merchantId'.getId,
      merchantOperatingCity = Just opCity,
      identifierType = Just DP.MOBILENUMBER,
      email = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing
    }

updateFleetOwnerInfo ::
  FOI.FleetOwnerInformation ->
  Common.FleetOwnerRegisterReqV2 ->
  Flow ()
updateFleetOwnerInfo fleetOwnerInfo Common.FleetOwnerRegisterReqV2 {..} = do
  now <- getCurrentTime
  let updFleetOwnerInfo =
        fleetOwnerInfo
          { FOI.fleetType = fromMaybe fleetOwnerInfo.fleetType (castFleetType <$> fleetType),
            FOI.registeredAt = Just now
          }
  void $ QFOI.updateByPrimaryKey updFleetOwnerInfo -- this update will backfill encrypted docs numbers

mkFleetOwnerVerifyReq ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Common.FleetOwnerVerifyReqV2 ->
  DRegistration.FleetOwnerLoginReq
mkFleetOwnerVerifyReq merchantShortId opCity Common.FleetOwnerVerifyReqV2 {..} =
  DRegistration.FleetOwnerLoginReq
    { merchantId = merchantShortId.getShortId,
      city = opCity,
      ..
    }

fleetOwnerVerify ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Common.FleetOwnerVerifyReqV2 ->
  Flow APISuccess
fleetOwnerVerify merchantShortId opCity req = do
  let h = DRegistration.FleetOwnerVerifyHandle {mkMobileNumberOtpKey = makeMobileNumberOtpKey}
  DRegistration.fleetOwnerVerifyHandler h $ mkFleetOwnerVerifyReq merchantShortId opCity req

makeMobileNumberOtpKey :: Text -> Text
makeMobileNumberOtpKey mobileNumber = "MobileNumberOtp:V2:mobileNumber-" <> mobileNumber
