module Domain.Action.Dashboard.Operator.FleetManagement
  ( getFleetManagementFleets,
    postFleetManagementFleetRegister,
    postFleetManagementFleetCreate,
    postFleetManagementFleetUnlink,
    postFleetManagementFleetLinkSendOtp,
    postFleetManagementFleetLinkVerifyOtp,
  )
where

import qualified API.Types.ProviderPlatform.Operator.FleetManagement as Common
import Control.Monad.Extra (mapMaybeM)
import qualified Domain.Action.Dashboard.Fleet.Registration as DRegistration
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import Domain.Types.FleetOperatorAssociation (FleetOperatorAssociation (fleetOwnerId))
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id, length)
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
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.MessageBuilder
  ( BuildFleetLinkUnlinkSuccessMessageReq (..),
    BuildOperatorJoiningMessageReq (..),
    buildFleetLinkSuccessMessage,
    buildFleetUnlinkSuccessMessage,
    buildOperatorJoiningMessage,
  )
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import Storage.Queries.FleetOperatorAssociationExtra (findAllActiveByOperatorIdWithLimitOffset)
import Storage.Queries.FleetOwnerInformationExtra
  ( findByPersonIdAndEnabledAndVerified,
  )
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Error
import qualified Tools.SMS as SMSHelper

getFleetManagementFleets ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.FleetInfoRes
getFleetManagementFleets _merchantShortId _opCity mbIsActive mbVerified mbLimit mbOffset requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  activeFleetOwnerLs <- findAllActiveByOperatorIdWithLimitOffset requestorId mbLimit mbOffset
  fleetOwnerInfoLs <-
    mapMaybeM (findByPersonIdAndEnabledAndVerified mbIsActive mbVerified . ID.Id . fleetOwnerId) activeFleetOwnerLs
  listItem <- mapM createFleetInfo fleetOwnerInfoLs
  let count = length listItem
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.FleetInfoRes {..}
  where
    createFleetInfo FOI.FleetOwnerInformation {..} = do
      totalVehicle <- VRCQuery.countAllActiveRCForFleet fleetOwnerPersonId.getId merchantId
      person <-
        QP.findById fleetOwnerPersonId
          >>= fromMaybeM (PersonDoesNotExist fleetOwnerPersonId.getId)
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM (InvalidRequest $ "Person do not have a mobile number " <> person.id.getId)
      pure $
        Common.FleetInfo
          { id = ID.cast fleetOwnerPersonId,
            name = person.firstName,
            isActive = enabled,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicleCount = totalVehicle,
            verified = verified
          }

postFleetManagementFleetCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Common.FleetOwnerCreateReq ->
  Environment.Flow Common.FleetOwnerCreateRes
postFleetManagementFleetCreate merchantShortId opCity requestorId req = do
  let enabled = Just True
  mkFleetOwnerRegisterRes <$> DRegistration.fleetOwnerLogin (Just requestorId) enabled (mkFleetOwnerLoginReq merchantShortId opCity req)

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
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postFleetManagementFleetRegister merchantShortId opCity requestorId req =
  DRegistration.fleetOwnerRegister (Just requestorId) $ mkFleetOwnerRegisterReq merchantShortId opCity req

mkFleetOwnerRegisterReq ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.FleetOwnerRegisterReq ->
  DRegistration.FleetOwnerRegisterReq
mkFleetOwnerRegisterReq merchantShortId opCity (Common.FleetOwnerRegisterReq {..}) = do
  DRegistration.FleetOwnerRegisterReq
    { personId = ID.cast @Common.Person @DP.Person personId,
      fleetType = castFleetType <$> fleetType,
      operatorReferralCode = Nothing,
      merchantId = merchantShortId.getShortId,
      city = opCity,
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
    mbFleetOwner <- B.runInReplica $ QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.FLEET_OWNER
    case mbFleetOwner of
      Just owner -> pure owner
      Nothing -> do
        let createReq =
              Common.FleetOwnerCreateReq
                { mobileNumber = req.mobileNumber,
                  mobileCountryCode = req.mobileCountryCode
                }
        let personAuth = DRegistration.buildFleetOwnerAuthReq merchant.id (mkFleetOwnerLoginReq merchantShortId opCity createReq)
        deploymentVersion <- asks (.version)
        personData <- DRegistration.createFleetOwnerDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion enabled
        pure personData

  SA.checkForFleetAssociationOverwrite merchant fleetOwner.id
  checkAssocOperator <- B.runInReplica $ QFOA.findByFleetOwnerIdAndOperatorId fleetOwner.id operator.id True
  when (isJust checkAssocOperator) $ throwError (InvalidRequest "Fleet already associated with operator")

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
