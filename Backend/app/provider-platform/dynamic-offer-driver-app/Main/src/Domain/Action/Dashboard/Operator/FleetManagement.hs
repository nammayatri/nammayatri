module Domain.Action.Dashboard.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetUnlink,
    postFleetManagementFleetLinkSendOtp,
    postFleetManagementFleetLinkVerifyOtp,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2 as Common
import qualified API.Types.ProviderPlatform.Operator.FleetManagement as Common
import Data.List.Extra (notNull)
import Domain.Action.Dashboard.Fleet.Onboarding
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DRegistrationV2
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant
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
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
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
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import Storage.Queries.FleetOperatorAssociationExtra (findAllActiveByOperatorIdWithLimitOffsetSearch)
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Error
import qualified Tools.SMS as SMSHelper

---------------------------------------------------------------------
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
  foaPersonFleetOwnerInfo <- findAllActiveByOperatorIdWithLimitOffsetSearch requestorId mbLimit mbOffset mbSearchString mbIsActive mbEnabled mbVerified
  listItem <- mapM createFleetInfo foaPersonFleetOwnerInfo
  let count = length listItem
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.FleetInfoRes {..}
  where
    createFleetInfo (foa, person, FOI.FleetOwnerInformation {..}) = do
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
            isActive = foa.isActive,
            enabled = enabled,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicleCount = totalVehicle,
            fleetType = Just $ DRegistrationV2.castFleetTypeToDomain fleetType,
            verified = verified,
            documents = statusRes,
            registeredAt = registeredAt
          }

---------------------------------------------------------------------
postFleetManagementFleetCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Text ->
  Common.FleetOwnerLoginReqV2 ->
  Environment.Flow Common.FleetOwnerLoginResV2
postFleetManagementFleetCreate merchantShortId opCity mbEnabled requestorId req = do
  DRegistrationV2.fleetOwnerLogin merchantShortId opCity (Just requestorId) mbEnabled req

---------------------------------------------------------------------
-- TODO should we remove?
postFleetManagementFleetRegister ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Common.FleetOwnerRegisterReqV2 ->
  Environment.Flow Common.FleetOwnerRegisterResV2
postFleetManagementFleetRegister = DRegistrationV2.postRegistrationV2Register

---------------------------------------------------------------------
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
  (mbSender, message, templateId) <-
    buildFleetUnlinkSuccessMessage merchantOpCityId $
      BuildFleetLinkUnlinkSuccessMessageReq
        { operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
        }
  let sender = fromMaybe smsCfg.sender mbSender
  SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult

  pure Kernel.Types.APISuccess.Success

---------------------------------------------------------------------
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
              Common.FleetOwnerLoginReqV2
                { mobileNumber = req.mobileNumber,
                  mobileCountryCode = req.mobileCountryCode
                }
        let personAuth = DRegistrationV2.buildFleetOwnerAuthReq merchant.id opCity createReq
        deploymentVersion <- asks (.version)
        personData <- DRegistrationV2.createFleetOwnerDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion enabled
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
    (mbSenderHeader, message, templateId) <-
      buildOperatorJoiningMessage merchantOpCityId $
        BuildOperatorJoiningMessageReq
          { operatorName = operatorName,
            otp = otpCode
          }
    let sender = fromMaybe smsCfg.sender mbSenderHeader
    SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult

  Redis.setExp key otpCode 3600
  pure $
    Common.FleetOwnerSendOtpRes
      { fleetOwnerId = cast fleetOwner.id,
        name = fleetOwner.firstName <> " " <> fromMaybe "" fleetOwner.lastName
      }

---------------------------------------------------------------------
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
  res <- try @_ @SomeException $ do
    (mbSender, message, templateId) <-
      buildFleetLinkSuccessMessage merchantOpCityId $
        BuildFleetLinkUnlinkSuccessMessageReq
          { operatorName = operator.firstName <> maybe "" (" " <>) operator.lastName
          }
    let sender = fromMaybe smsCfg.sender mbSender
    SMSHelper.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId) >>= Sms.checkSmsResult
  case res of
    Left err -> logError $ "Failed to send sms about fleet link. Please check templates: " <> show err
    Right () -> pure ()

  Redis.del key
  pure Kernel.Types.APISuccess.Success

makeFleetLinkOtpKey :: Text -> Text
makeFleetLinkOtpKey phoneNo = "Fleet:Link:PhoneNo:" <> phoneNo
