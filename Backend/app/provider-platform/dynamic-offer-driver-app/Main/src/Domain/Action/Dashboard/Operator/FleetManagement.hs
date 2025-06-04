module Domain.Action.Dashboard.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetUnlink,
    postFleetManagementFleetLinkSendOtp,
    postFleetManagementFleetLinkVerifyOtp,
    createFleetOwnerInfo,
    castFleetType,
  )
where

import qualified API.Types.ProviderPlatform.Operator.FleetManagement as Common
import Data.List.Extra (notNull)
import Domain.Action.Dashboard.Fleet.Onboarding
import qualified Domain.Action.Dashboard.Fleet.Registration as DRegistration
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.DriverReferral as DR
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (any, id, length, null, whenJust)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.SMS as Sms
import Kernel.Prelude
import Kernel.Sms.Config (useFakeSms)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.MessageBuilder
  ( BuildFleetLinkUnlinkSuccessMessageReq (..),
    BuildOperatorJoiningMessageReq (..),
    buildFleetLinkSuccessMessage,
    buildFleetUnlinkSuccessMessage,
    buildOperatorJoiningMessage,
  )
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Cac.TransporterConfig
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverGstin as QGST
import qualified Storage.Queries.DriverPanCard as QPanCard
import Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import Storage.Queries.FleetOperatorAssociationExtra (findAllActiveByOperatorIdWithLimitOffsetSearch)
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Error
import qualified Tools.SMS as SMSHelper

getFleetManagementFleets ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.FleetInfoRes
getFleetManagementFleets merchantShortId opCity mbIsActive mbVerified mbEnabled mbLimit mbOffset mbSearchString requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  (_, personLs, fleetOwnerInfoLs) <- unzip3 <$> findAllActiveByOperatorIdWithLimitOffsetSearch requestorId mbLimit mbOffset mbSearchString mbIsActive mbEnabled mbVerified
  listItem <- mapM createFleetInfo (zip fleetOwnerInfoLs personLs)
  let count = length listItem
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.FleetInfoRes {..}
  where
    createFleetInfo (FOI.FleetOwnerInformation {..}, person) = do
      now <- getCurrentTime
      totalVehicle <- VRCQuery.countAllRCForFleet fleetOwnerPersonId.getId merchantId
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM (InvalidRequest $ "Person do not have a mobile number " <> person.id.getId)
      merchant <- findMerchantByShortId merchantShortId
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
      transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
      driverImages <- IQuery.findAllByPersonId transporterConfig person.id
      let driverImagesInfo = IQuery.DriverImagesInfo {driverId = person.id, merchantOperatingCity = merchantOpCity, driverImages, transporterConfig, now}
      statusRes <-
        castStatusRes
          <$> SStatus.statusHandler' driverImagesInfo Nothing Nothing Nothing Nothing Nothing (Just True)
      pure $
        Common.FleetInfo
          { id = ID.cast fleetOwnerPersonId,
            name = person.firstName,
            isActive = enabled,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicleCount = totalVehicle,
            verified = verified,
            documents = statusRes
          }

postFleetManagementFleetCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Common.FleetOwnerCreateReq ->
  Environment.Flow Common.FleetOwnerCreateRes
postFleetManagementFleetCreate merchantShortId opCity requestorId req = do
  let enabled = Just True
  mkFleetOwnerRegisterRes <$> fleetOwnerLogin (Just requestorId) enabled (mkFleetOwnerLoginReq merchantShortId opCity req)

-- As Domain.Action.Dashboard.Fleet.Registration.fleetOwnerLogin function was reverted, its logic moved here.
-- Code duplication can be refactored later
fleetOwnerLogin ::
  Maybe Text ->
  Maybe Bool ->
  DRegistration.FleetOwnerLoginReq ->
  Environment.Flow DRegistration.FleetOwnerRegisterRes
fleetOwnerLogin _mbRequestorId enabled req = do
  runRequestValidation DRegistration.validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
      merchantId = ShortId req.merchantId
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just req.city)
  mobileNumberHash <- getDbHash mobileNumber
  mbPerson <- QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR]
  personId <- case mbPerson of
    Just person -> pure person.id
    Nothing -> do
      -- Operator won't reach here as it has separate sign up flow --
      let personAuth = buildFleetOwnerAuthReq merchant.id req
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
        SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  let key = DRegistration.makeMobileNumberOtpKey mobileNumber
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  void $ Redis.setExp key otp expTime
  pure $ DRegistration.FleetOwnerRegisterRes {personId = personId.getId}

buildFleetOwnerAuthReq ::
  Id DMerchant.Merchant ->
  DRegistration.FleetOwnerLoginReq ->
  Registration.AuthReq
buildFleetOwnerAuthReq merchantId' DRegistration.FleetOwnerLoginReq {..} =
  Registration.AuthReq
    { name = Just "Fleet Owner", -- to be updated in register
      mobileNumber = Just mobileNumber,
      mobileCountryCode = Just mobileCountryCode,
      merchantId = merchantId'.getId,
      merchantOperatingCity = Just city,
      identifierType = Just DP.MOBILENUMBER,
      email = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing
    }

updateFleetOwnerInfo ::
  FOI.FleetOwnerInformation ->
  FleetOwnerRegisterReq ->
  Environment.Flow ()
updateFleetOwnerInfo fleetOwnerInfo FleetOwnerRegisterReq {..} = do
  let updFleetOwnerInfo =
        fleetOwnerInfo
          { FOI.fleetType = fromMaybe fleetOwnerInfo.fleetType fleetType,
            FOI.gstNumber = fleetOwnerInfo.gstNumber,
            FOI.gstImageId = fleetOwnerInfo.gstImageId,
            FOI.businessLicenseImageId = businessLicenseImage
          }
  void $ QFOI.updateByPrimaryKey updFleetOwnerInfo

createFleetOwnerDetails :: Registration.AuthReq -> Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Maybe Bool -> Environment.Flow DP.Person
createFleetOwnerDetails authReq merchantId merchantOpCityId isDashboard deploymentVersion enabled = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- Registration.makePerson authReq transporterConfig Nothing Nothing Nothing Nothing (Just deploymentVersion) merchantId merchantOpCityId isDashboard (Just DP.FLEET_OWNER)
  void $ QP.create person
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  QDriverStats.createInitialDriverStats merchantOperatingCity.currency merchantOperatingCity.distanceUnit person.id
  fork "creating fleet owner info" $ createFleetOwnerInfo person.id merchantId enabled
  pure person

createFleetOwnerInfo :: Id DP.Person -> Id DMerchant.Merchant -> Maybe Bool -> Environment.Flow ()
createFleetOwnerInfo personId merchantId enabled = do
  now <- getCurrentTime
  let fleetOwnerInfo =
        FOI.FleetOwnerInformation
          { fleetOwnerPersonId = personId,
            merchantId = merchantId,
            fleetType = FOI.NORMAL_FLEET, -- overwrite in register
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
            panImageId = Nothing,
            panNumber = Nothing,
            panNumberDec = Nothing,
            aadhaarBackImageId = Nothing,
            aadhaarFrontImageId = Nothing,
            aadhaarNumber = Nothing,
            aadhaarNumberDec = Nothing,
            createdAt = now,
            updatedAt = now,
            registeredAt = Nothing
          }
  QFOI.create fleetOwnerInfo

mkFleetOwnerLoginReq ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.FleetOwnerCreateReq ->
  DRegistration.FleetOwnerLoginReq
mkFleetOwnerLoginReq merchantShortId opCity (Common.FleetOwnerCreateReq {..}) = do
  DRegistration.FleetOwnerLoginReq
    { otp = Nothing,
      merchantId = merchantShortId.getShortId,
      city = opCity,
      ..
    }

mkFleetOwnerRegisterRes ::
  DRegistration.FleetOwnerRegisterRes ->
  Common.FleetOwnerCreateRes
mkFleetOwnerRegisterRes DRegistration.FleetOwnerRegisterRes {..} =
  Common.FleetOwnerCreateRes {personId = ID.Id personId}

postFleetManagementFleetRegister ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Common.FleetOwnerRegisterReq ->
  Environment.Flow Common.FleetOwnerUpdateRes
postFleetManagementFleetRegister merchantShortId opCity requestorId req = do
  res <- fleetOwnerRegister (Just requestorId) $ mkFleetOwnerRegisterReq merchantShortId opCity req
  pure $ Common.FleetOwnerUpdateRes {enabled = res.enabled}

-- As Domain.Action.Dashboard.Fleet.Registration.fleetOwnerRegister function was reverted, its logic moved here.
-- Code duplication can be refactored later
fleetOwnerRegister :: Maybe Text -> FleetOwnerRegisterReq -> Environment.Flow DRegistration.FleetOwnerUpdateRes
fleetOwnerRegister mbRequestorId req = do
  let fleetOwnerId = req.personId.getId
  person <- QP.findById req.personId >>= fromMaybeM (PersonDoesNotExist fleetOwnerId)
  fleetOwnerInfo <- QFOI.findByPrimaryKey req.personId >>= fromMaybeM (PersonDoesNotExist req.personId.getId)
  void $ QP.updateByPrimaryKey person{firstName = req.firstName, lastName = Just req.lastName}
  void $ updateFleetOwnerInfo fleetOwnerInfo req
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)

  mbRequestedOperatorId <- case mbRequestorId of
    Just requestorId -> do
      requestor <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
      if (requestor.role == DP.OPERATOR) then pure (Just requestor.id.getId) else pure Nothing
    Nothing -> pure Nothing

  mbReferredOperatorId <- DRegistration.getOperatorIdFromReferralCode req.operatorReferralCode
  whenJust (mbReferredOperatorId <|> mbRequestedOperatorId) $ \referredOperatorId -> do
    fleetAssocs <- QFOA.findAllFleetAssociations req.personId.getId
    when (null fleetAssocs) $ do
      fleetOperatorAssocData <- SA.makeFleetOperatorAssociation person.merchantId person.merchantOperatingCityId (req.personId.getId) referredOperatorId (DomainRC.convertTextToUTC (Just "2099-12-12"))
      QFOA.create fleetOperatorAssocData
      DOR.incrementOnboardedCount DOR.FleetReferral (Id referredOperatorId) transporterConfig
  when (transporterConfig.generateReferralCodeForFleet == Just True) $ do
    fleetReferral <- QDR.findById person.id
    when (isNothing fleetReferral) $ void $ DR.generateReferralCode (Just DP.FLEET_OWNER) (req.personId, person.merchantId, person.merchantOperatingCityId)
  fork "Uploading Business License Image" $ do
    whenJust req.businessLicenseImage $ \businessLicenseImage -> do
      let req' = Image.ImageValidateRequest {imageType = DVC.BusinessLicense, image = businessLicenseImage, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
      image <- Image.validateImage True (req.personId, person.merchantId, person.merchantOperatingCityId) req'
      QFOI.updateBusinessLicenseImage (Just image.imageId.getId) req.personId
  enabled <- enableFleetIfPossible
  return $ DRegistration.FleetOwnerUpdateRes enabled
  where
    enableFleetIfPossible :: Environment.Flow Bool
    enableFleetIfPossible = do
      if (req.adminApprovalRequired /= Just True)
        then do
          panCard <- QPanCard.findByDriverId req.personId
          gstIn <- QGST.findByDriverId req.personId
          case req.fleetType of
            Just FOI.NORMAL_FLEET ->
              case panCard of
                Just pan | pan.verificationStatus == Documents.VALID -> do
                  void $ QFOI.updateFleetOwnerEnabledStatus True req.personId
                  pure True
                _ -> pure False
            Just FOI.BUSINESS_FLEET ->
              case (panCard, gstIn) of
                (Just pan, Just gst)
                  | pan.verificationStatus == Documents.VALID
                      && gst.verificationStatus == Documents.VALID -> do
                    void $ QFOI.updateFleetOwnerEnabledStatus True req.personId
                    pure True
                _ -> pure False
            _ -> pure False
        else pure False

-- TODO refactor duplicated type
data FleetOwnerRegisterReq = FleetOwnerRegisterReq
  { firstName :: Text,
    lastName :: Text,
    personId :: Id DP.Person,
    merchantId :: Text,
    email :: Maybe Text,
    city :: City.City,
    fleetType :: Maybe FOI.FleetType,
    panNumber :: Maybe Text,
    gstNumber :: Maybe Text,
    businessLicenseNumber :: Maybe Text,
    panImageId1 :: Maybe Text,
    panImageId2 :: Maybe Text,
    gstCertificateImage :: Maybe Text,
    businessLicenseImage :: Maybe Text,
    operatorReferralCode :: Maybe Text,
    adminApprovalRequired :: Maybe Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

mkFleetOwnerRegisterReq ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.FleetOwnerRegisterReq ->
  FleetOwnerRegisterReq
mkFleetOwnerRegisterReq merchantShortId opCity (Common.FleetOwnerRegisterReq {..}) = do
  FleetOwnerRegisterReq
    { personId = ID.cast @Common.Person @DP.Person personId,
      fleetType = castFleetType <$> fleetType,
      operatorReferralCode = Nothing,
      merchantId = merchantShortId.getShortId,
      city = opCity,
      adminApprovalRequired = Nothing, -- to be updated in fleetOwnerRegister
      ..
    }

castFleetType :: Common.FleetType -> FOI.FleetType
castFleetType = \case
  Common.RENTAL_FLEET -> FOI.RENTAL_FLEET
  Common.NORMAL_FLEET -> FOI.NORMAL_FLEET
  Common.BUSINESS_FLEET -> FOI.BUSINESS_FLEET

checkOperator :: Text -> Environment.Flow DP.Person
checkOperator requestorId = do
  operator <- B.runInReplica $ QP.findById (Id requestorId :: Id DP.Person) >>= fromMaybeM (PersonNotFound requestorId)
  unless (operator.role == DP.OPERATOR) $
    throwError AccessDenied
  pure operator

postFleetManagementFleetUnlink ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetUnlink merchantShortId opCity fleetOwnerId requestorId = do
  operator <- checkOperator requestorId

  fleetOwner <- B.runInReplica $ QP.findById (Id fleetOwnerId :: Id DP.Person) >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId)
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")

  QFOA.endFleetOperatorAssociation fleetOwner.id operator.id

  decryptedMobileNumber <-
    mapM decrypt fleetOwner.mobileNumber
      >>= fromMaybeM (InvalidRequest $ "Person does not have a mobile number " <> getId fleetOwner.id)
  let phoneNumber = fromMaybe "+91" fleetOwner.mobileCountryCode <> decryptedMobileNumber
  smsCfg <- asks (.smsCfg)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  (mbSender, message) <-
    buildFleetUnlinkSuccessMessage merchantOpCityId $
      BuildFleetLinkUnlinkSuccessMessageReq
        { operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
        }
  let sender = fromMaybe smsCfg.sender mbSender
  SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult

  pure Kernel.Types.APISuccess.Success

postFleetManagementFleetLinkSendOtp ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Common.FleetOwnerSendOtpReq ->
  Environment.Flow Common.FleetOwnerSendOtpRes
postFleetManagementFleetLinkSendOtp merchantShortId opCity requestorId req = do
  operator <- checkOperator requestorId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mobileNumberHash <- getDbHash req.mobileNumber
  let enabled = Just True
  fleetOwner <- do
    mbPerson <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR]
    mbFleetOwner <- forM mbPerson $ \person -> case person.role of
      DP.FLEET_OWNER -> pure person
      _ -> throwError (InvalidRequest "Person should be fleet owner")

    case mbFleetOwner of
      Just owner -> pure owner
      Nothing -> do
        let createReq =
              Common.FleetOwnerCreateReq
                { mobileNumber = req.mobileNumber,
                  mobileCountryCode = req.mobileCountryCode
                }
        let personAuth = buildFleetOwnerAuthReq merchant.id (mkFleetOwnerLoginReq merchantShortId opCity createReq)
        deploymentVersion <- asks (.version)
        personData <- createFleetOwnerDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion enabled
        pure personData

  existingFOAssociations <- QFOA.findAllByFleetOwnerId fleetOwner.id True
  when (any (\foa -> Id foa.fleetOwnerId == fleetOwner.id && Id foa.operatorId == operator.id) existingFOAssociations)
    . throwError
    $ InvalidRequest "Fleet already associated with operator"
  when (merchant.overwriteAssociation /= Just True && notNull existingFOAssociations)
    . throwError
    $ InvalidRequest "Fleet already associated with another operator"

  smsCfg <- asks (.smsCfg)
  let mbUseFakeOtp = (show <$> useFakeSms smsCfg) <|> fleetOwner.useFakeOtp
      phoneNumber = req.mobileCountryCode <> req.mobileNumber
      key = makeFleetLinkOtpKey phoneNumber
  otpCode <- maybe generateOTPCode return mbUseFakeOtp
  whenNothing_ mbUseFakeOtp $ do
    let operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
    (mbSenderHeader, message) <-
      buildOperatorJoiningMessage merchantOpCityId $
        BuildOperatorJoiningMessageReq
          { operatorName = operatorName,
            otp = otpCode
          }
    let sender = fromMaybe smsCfg.sender mbSenderHeader
    SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult

  Redis.setExp key otpCode 3600
  pure $
    Common.FleetOwnerSendOtpRes
      { fleetOwnerId = cast fleetOwner.id,
        name = fleetOwner.firstName <> " " <> fromMaybe "" fleetOwner.lastName
      }

postFleetManagementFleetLinkVerifyOtp ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Common.FleetOwnerVerifyOtpReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetLinkVerifyOtp merchantShortId opCity requestorId req = do
  operator <- checkOperator requestorId
  fleetOwner <- B.runInReplica $ QP.findById (cast req.fleetOwnerId) >>= fromMaybeM (FleetOwnerNotFound (getId req.fleetOwnerId))
  unless (fleetOwner.role == DP.FLEET_OWNER) $
    throwError (InvalidRequest "Invalid fleet owner")

  checkAssocOperator <- B.runInReplica $ QFOA.findByFleetOwnerIdAndOperatorId fleetOwner.id operator.id True
  when (isJust checkAssocOperator) $ throwError (InvalidRequest "Fleet already associated with operator")

  decryptedMobileNumber <- mapM decrypt fleetOwner.mobileNumber >>= fromMaybeM (InvalidRequest $ "Person does not have a mobile number " <> getId fleetOwner.id)
  let key = makeFleetLinkOtpKey (fromMaybe "+91" fleetOwner.mobileCountryCode <> decryptedMobileNumber)
  storedOtp <- Redis.get key >>= fromMaybeM OtpNotFound
  unless (storedOtp == req.otp) $ throwError InvalidOtp

  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  SA.endFleetAssociationsIfAllowed merchant merchantOpCityId fleetOwner
  fleetOperatorAssociation <- SA.makeFleetOperatorAssociation merchant.id merchantOpCityId (getId fleetOwner.id) operator.id.getId (DomainRC.convertTextToUTC (Just "2099-12-12"))
  QFOA.create fleetOperatorAssociation

  let phoneNumber = fromMaybe "+91" fleetOwner.mobileCountryCode <> decryptedMobileNumber
  smsCfg <- asks (.smsCfg)
  (mbSender, message) <-
    buildFleetLinkSuccessMessage merchantOpCityId $
      BuildFleetLinkUnlinkSuccessMessageReq
        { operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
        }
  let sender = fromMaybe smsCfg.sender mbSender
  SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
  Redis.del key
  pure Kernel.Types.APISuccess.Success

makeFleetLinkOtpKey :: Text -> Text
makeFleetLinkOtpKey phoneNo = "Fleet:Link:PhoneNo:" <> phoneNo
