{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Registration
  ( fleetOwnerRegister,
    fleetOwnerLogin,
    FleetOwnerLoginReq (..),
    FleetOwnerRegisterReq (..),
    FleetOwnerRegisterRes (..),
    fleetOwnerVerify,
    FleetOwnerVerifyHandle (..),
    FleetOwnerVerifyRes (..),
    fleetOwnerVerifyHandler,
  )
where

import qualified API.Types.UI.DriverOnboardingV2 as DO
import Data.OpenApi (ToSchema)
import Domain.Action.Dashboard.Fleet.Referral
import qualified Domain.Action.Internal.DriverMode as DriverMode
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import qualified Domain.Action.UI.DriverOnboardingV2 as Registration
import qualified Domain.Action.UI.DriverReferral as DR
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.DocumentVerificationConfig as DVC
import Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Kernel.Utils.Validation
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding as DomainRC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.SMS as Sms hiding (Success)

---------------------------------------------------------------------
data FleetOwnerLoginReq = FleetOwnerLoginReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: Text,
    otp :: Maybe Text,
    city :: Context.City
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data FleetOwnerRegisterReq = FleetOwnerRegisterReq
  { firstName :: Text,
    lastName :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
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
    adminApprovalRequired :: Maybe Bool,
    ticketPlaceId :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

newtype FleetOwnerRegisterRes = FleetOwnerRegisterRes
  { personId :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

newtype FleetOwnerVerifyRes = FleetOwnerVerifyRes
  { authToken :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

fleetOwnerRegister :: FleetOwnerRegisterReq -> Maybe Bool -> Flow FleetOwnerRegisterRes
fleetOwnerRegister req mbEnabled = do
  let merchantId = ShortId req.merchantId
  mobileNumberHash <- getDbHash req.mobileNumber
  deploymentVersion <- asks (.version)
  merchant <-
    QMerchant.findByShortId merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just req.city)
  let personAuth = buildFleetOwnerAuthReq merchant.id req
  personOpt <- QP.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileNumberHash merchant.id DP.FLEET_OWNER
  case personOpt of
    Just pData -> throwError $ UserAlreadyExists pData.id.getId
    Nothing -> do
      maybeOperatorId <- getOperatorIdFromReferralCode req.operatorReferralCode
      person <- createFleetOwnerDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion req.fleetType mbEnabled req.gstNumber maybeOperatorId req.ticketPlaceId
      fork "Creating Pan Info for Fleet Owner" $ do
        createPanInfo person.id merchant.id merchantOpCityId req.panImageId1 req.panImageId2 req.panNumber
      fork "Uploading GST Image" $ do
        whenJust req.gstCertificateImage $ \gstImage -> do
          let req' = Image.ImageValidateRequest {imageType = DVC.GSTCertificate, image = gstImage, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
          image <- Image.validateImage True (person.id, merchant.id, merchantOpCityId) req'
          gstNumber <- forM req.gstNumber encrypt
          QFOI.updateGstImage gstNumber (Just image.imageId.getId) person.id
      return $ FleetOwnerRegisterRes {personId = person.id.getId}

getOperatorIdFromReferralCode :: Maybe Text -> Flow (Maybe Text)
getOperatorIdFromReferralCode Nothing = return Nothing
getOperatorIdFromReferralCode (Just refCode) = do
  let referralReq = FleetReferralReq {value = refCode}
  result <- isValidReferralForRole referralReq DP.OPERATOR
  case result of
    SuccessCode val -> return $ Just val

createFleetOwnerDetails :: Registration.AuthReq -> Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Maybe FOI.FleetType -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Flow DP.Person
createFleetOwnerDetails authReq merchantId merchantOpCityId isDashboard deploymentVersion mbfleetType mbEnabled mbgstNumber mbReferredOperatorId mbTicketPlaceId = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- Registration.makePerson authReq transporterConfig Nothing Nothing Nothing Nothing Nothing (Just deploymentVersion) merchantId merchantOpCityId isDashboard (Just DP.FLEET_OWNER)
  void $ QP.create person
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  QDriverStats.createInitialDriverStats merchantOperatingCity.currency merchantOperatingCity.distanceUnit person.id
  createFleetOwnerInfo person.id merchantId mbfleetType mbEnabled mbgstNumber mbReferredOperatorId mbTicketPlaceId
  whenJust mbReferredOperatorId $ \referredOperatorId -> do
    fleetOperatorAssData <- SA.makeFleetOperatorAssociation merchantId merchantOpCityId (person.id.getId) referredOperatorId (DomainRC.convertTextToUTC (Just "2099-12-12"))
    QFOA.create fleetOperatorAssData
    let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
    when allowCacheDriverFlowStatus $ do
      DriverMode.incrementOperatorStatusKeyForFleetOwner referredOperatorId person.id.getId
    DOR.incrementOnboardedCount DOR.FleetReferral (Id referredOperatorId) transporterConfig
  when (transporterConfig.generateReferralCodeForFleet == Just True) $ do
    void $ DR.generateReferralCode (Just DP.FLEET_OWNER) (person.id, merchantId, merchantOpCityId)
  pure person

createPanInfo :: Id DP.Person -> Id DMerchant.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maybe Text -> Maybe Text -> Flow ()
createPanInfo personId merchantId merchantOperatingCityId (Just img1) _ (Just panNo) = do
  let req' = Image.ImageValidateRequest {imageType = DVC.PanCard, image = img1, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
  image <- Image.validateImage True (personId, merchantId, merchantOperatingCityId) req'
  let panReq = DO.DriverPanReq {panNumber = panNo, imageId1 = image.imageId, imageId2 = Nothing, consent = True, nameOnCard = Nothing, dateOfBirth = Nothing, consentTimestamp = Nothing, validationStatus = Nothing, verifiedBy = Nothing, transactionId = Nothing, nameOnGovtDB = Nothing, docType = Nothing}
  void $ Registration.postDriverRegisterPancardHelper (Just personId, merchantId, merchantOperatingCityId) True panReq
createPanInfo _ _ _ _ _ _ = pure () --------- currently we can have it like this as Pan info is optional

createFleetOwnerInfo :: Id DP.Person -> Id DMerchant.Merchant -> Maybe FOI.FleetType -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Flow ()
createFleetOwnerInfo personId merchantId mbFleetType mbEnabled mbGstNumber mbReferredByOperatorId mbTicketPlaceId = do
  now <- getCurrentTime
  mbGstNumberEnc <- forM mbGstNumber encrypt
  let fleetType = fromMaybe NORMAL_FLEET mbFleetType
      enabled = fromMaybe True mbEnabled
      fleetOwnerInfo =
        FOI.FleetOwnerInformation
          { fleetOwnerPersonId = personId,
            merchantId = merchantId,
            fleetType = fleetType,
            enabled = enabled, ------ currently we are not validating any fleet owner Document there fore marking it as true
            blocked = False,
            verified = False,
            gstNumber = mbGstNumberEnc,
            gstNumberDec = Nothing,
            gstImageId = Nothing,
            businessLicenseImageId = Nothing,
            businessLicenseNumber = Nothing,
            businessLicenseNumberDec = Nothing,
            referredByOperatorId = mbReferredByOperatorId,
            panImageId = Nothing,
            panNumber = Nothing,
            panNumberDec = Nothing,
            aadhaarBackImageId = Nothing,
            aadhaarFrontImageId = Nothing,
            aadhaarNumber = Nothing,
            aadhaarNumberDec = Nothing,
            createdAt = now,
            updatedAt = now,
            registeredAt = Nothing,
            isEligibleForSubscription = True,
            ticketPlaceId = mbTicketPlaceId,
            lienAmount = Nothing,
            prepaidSubscriptionBalance = Nothing,
            planExpiryDate = Nothing
          }
  QFOI.create fleetOwnerInfo

fleetOwnerLogin ::
  FleetOwnerLoginReq ->
  Flow APISuccess
fleetOwnerLogin req = do
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
      merchantId = ShortId req.merchantId

  sendOtpRateLimitOptions <- asks (.sendOtpRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeMobileNumberHitsCountKey mobileNumber) sendOtpRateLimitOptions

  merchant <-
    QMerchant.findByShortId merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
  mobileNumberHash <- getDbHash mobileNumber
  person <-
    QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR]
      >>= fromMaybeM (PersonNotFound mobileNumber)
  when (person.role == DP.FLEET_OWNER) $ do
    fleetOwnerInfo <- QFOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
    unless fleetOwnerInfo.enabled (throwError $ InvalidRequest "fleetOwner is not enabled")
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just req.city)
  let useFakeOtpM = useFakeSms smsCfg
  otp <- maybe generateOTPCode (return . show) useFakeOtpM
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let otpCode = otp
        phoneNumber = countryCode <> mobileNumber
    withLogTag ("mobileNumber" <> req.mobileNumber) $
      do
        (mbSender, message, templateId) <-
          MessageBuilder.buildSendOTPMessage merchantOpCityId $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        let sender = fromMaybe smsCfg.sender mbSender
        Sms.sendSMS merchant.id merchantOpCityId (Sms.SendSMSReq message phoneNumber sender templateId)
          >>= Sms.checkSmsResult
  let key = makeMobileNumberOtpKey mobileNumber
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp key otp expTime
  pure Success

buildFleetOwnerAuthReq ::
  Id DMerchant.Merchant ->
  FleetOwnerRegisterReq ->
  Registration.AuthReq
buildFleetOwnerAuthReq merchantId' FleetOwnerRegisterReq {..} =
  Registration.AuthReq
    { name = Just (firstName <> " " <> lastName),
      mobileNumber = Just mobileNumber,
      mobileCountryCode = Just mobileCountryCode,
      merchantId = merchantId'.getId,
      merchantOperatingCity = Just city,
      identifierType = Just DP.MOBILENUMBER,
      email = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing
    }

fleetOwnerVerify ::
  FleetOwnerLoginReq ->
  Flow APISuccess
fleetOwnerVerify req = do
  let h = FleetOwnerVerifyHandle {mkMobileNumberOtpKey = makeMobileNumberOtpKey}
  fleetOwnerVerifyHandler h req

newtype FleetOwnerVerifyHandle = FleetOwnerVerifyHandle
  { mkMobileNumberOtpKey :: Text -> Text
  }

fleetOwnerVerifyHandler ::
  FleetOwnerVerifyHandle ->
  FleetOwnerLoginReq ->
  Flow APISuccess
fleetOwnerVerifyHandler h req = do
  case req.otp of
    Just otp -> do
      mobileNumberOtpKey <- Redis.safeGet $ h.mkMobileNumberOtpKey req.mobileNumber
      case mobileNumberOtpKey of
        Just otpHash -> do
          unless (otpHash == otp) $ throwError InvalidAuthData
          let merchantId = ShortId req.merchantId
          merchant <-
            QMerchant.findByShortId merchantId
              >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
          mobileNumberHash <- getDbHash req.mobileNumber
          person <- QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.FLEET_OWNER, DP.OPERATOR] >>= fromMaybeM (PersonNotFound req.mobileNumber)
          -- currently we don't create fleetOwnerInfo for operator
          when (person.role == DP.FLEET_OWNER) $
            void $ QFOI.updateFleetOwnerVerifiedStatus True person.id
          pure Success
        Nothing -> throwError InvalidAuthData
    _ -> throwError InvalidAuthData

makeMobileNumberOtpKey :: Text -> Text
makeMobileNumberOtpKey mobileNumber = "MobileNumberOtp:mobileNumber-" <> mobileNumber

makeMobileNumberHitsCountKey :: Text -> Text
makeMobileNumberHitsCountKey mobileNumber = "MobileNumberOtp:mobileNumberHits-" <> mobileNumber <> ":hitsCount"

validateInitiateLoginReq :: Validate FleetOwnerLoginReq
validateInitiateLoginReq FleetOwnerLoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]
