{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE.See the GNU Affero General Public License for more details.You should have received a copy of

 the GNU Affero General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Registration
  ( AuthReq (..),
    AuthRes (..),
    AuthWithOtpRes (..),
    ResendAuthRes,
    AuthVerifyReq (..),
    AuthVerifyRes (..),
    MarketingParams (..),
    MarketEventReq (..),
    auth,
    authWithOtp,
    verify,
    resend,
    logout,
    cleanCachedTokens,
    createDriverWithDetails,
    makePerson,
    marketingEventsPreLogin,
    marketingEventsPostLogin,
    signatureAuth,
  )
where

import BecknV2.FRFS.Enums (VehicleCategory (BUS))
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.OpenApi hiding (email, info, name, password, url)
import Data.Text hiding (elem)
import qualified Data.Text as T
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import Domain.Action.UI.DriverReferral
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DriverFlowStatus as DriverFlowStatus
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Extra.Plan as DEP
import qualified Domain.Types.Merchant as DO
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.TransporterConfig as TC
import qualified Email.Types
import Environment (Flow)
-- import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common as BC
import Kernel.Types.HideSecrets (HideSecrets (..))
import Kernel.Types.Id
import qualified Kernel.Types.Predicate as P
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import Kernel.Utils.Version
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types (GimsEmployeeLoginReq (..), GimsEmployeeRole (..))
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.IntegratedBPPConfig (findFirstIbppConfigByCityAndVehicle, getGimsBaseUrl)
import qualified SharedLogic.OTP as SOTP
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverInformation as QD
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified System.Environment as SE
import qualified Text.Hex as Hex
import Tools.Auth (authTokenCacheKey, decryptAES128)
import Tools.Error
import qualified Tools.Event as TE
import Tools.MarketingEvents as TM
import Tools.Whatsapp as Whatsapp

data AuthReq = AuthReq
  { mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCity :: Maybe Context.City,
    email :: Maybe Text,
    password :: Maybe Text,
    name :: Maybe Text,
    identifierType :: Maybe SP.IdentifierType,
    registrationLat :: Maybe Double,
    registrationLon :: Maybe Double,
    otpChannel :: Maybe SOTP.OTPChannel
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

instance HideSecrets AuthReq where
  type ReqWithoutSecrets AuthReq = AuthReq
  hideSecrets (req :: AuthReq) = req {email = "***" <$ req.email, password = "***" <$ req.password}

validateInitiateLoginReq :: Validate AuthReq
validateInitiateLoginReq AuthReq {..} =
  sequenceA_
    [ whenJust mobileNumber $ \justMobileNumber -> validateField "mobileNumber" justMobileNumber P.mobileNumber,
      whenJust mobileCountryCode $ \countryCode -> validateField "mobileCountryCode" countryCode P.mobileCountryCode
    ]

data AuthRes = AuthRes
  { authId :: Id SR.RegistrationToken,
    attempts :: Int,
    token :: Maybe Text,
    person :: Maybe SP.PersonAPIEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data AuthWithOtpRes = AuthWithOtpRes
  { authId :: Id SR.RegistrationToken,
    otpCode :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema)

type ResendAuthRes = AuthRes

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: FCMRecipientToken,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

validateAuthVerifyReq :: Validate AuthVerifyReq
validateAuthVerifyReq AuthVerifyReq {..} =
  sequenceA_
    [ validateField "otp" otp $ P.ExactLength 4 `P.And` P.star P.digit
    ]

data AuthVerifyRes = AuthVerifyRes
  { token :: Text,
    person :: SP.PersonAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data MarketingParams = MarketingParams
  { gclId :: Maybe Text,
    utmCampaign :: Maybe Text,
    utmContent :: Maybe Text,
    utmCreativeFormat :: Maybe Text,
    utmMedium :: Maybe Text,
    utmSource :: Maybe Text,
    utmTerm :: Maybe Text,
    appName :: Maybe Text,
    userType :: Maybe TE.UserType
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data MarketEventReq = MarketEventReq
  { marketingParams :: MarketingParams
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

authHitsCountKey :: SP.Person -> Text
authHitsCountKey person = "BPP:Registration:auth:" <> getId person.id <> ":hitsCount"

auth ::
  Bool ->
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow AuthRes
auth isDashboard req' mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash mbXForwardedFor = do
  case fromMaybe SP.MOBILENUMBER req'.identifierType of
    SP.GIMS_EMAIL_PASSWORD -> do
      email <- req'.email & fromMaybeM (InvalidRequest "Email is required for GIMS_EMAIL_PASSWORD auth")
      password <- req'.password & fromMaybeM (InvalidRequest "Password is required for GIMS_EMAIL_PASSWORD auth")
      smsCfg <- asks (.smsCfg)
      deploymentVersion <- asks (.version)
      mbCloudType <- asks (.cloudType)
      let merchantId = Id req'.merchantId :: Id DO.Merchant
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant req'.merchantOperatingCity
      integratedBPPConfig <- findFirstIbppConfigByCityAndVehicle merchantOpCityId (show BUS)
      baseUrl <- getGimsBaseUrl integratedBPPConfig
      let gtfsId = integratedBPPConfig.feedKey
      emailDbHash <- getDbHash email
      passwordDbHash <- getDbHash password
      let emailHashHex = Hex.encodeHex (unDbHash emailDbHash)
          passwordHashHex = Hex.encodeHex (unDbHash passwordDbHash)
      gimsResp <-
        NandiFlow.gimsEmployeeLogin
          baseUrl
          gtfsId
          GimsEmployeeLoginReq
            { auth_type = Just "Email",
              email_hash = emailHashHex,
              password_hash = passwordHashHex
            }
      unless gimsResp.verified $ throwError $ InvalidRequest "GIMS verification failed"
      operatorBadgeToken <- gimsResp.token & fromMaybeM (InvalidRequest "GIMS did not return operator badge token")
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let loginRole = case gimsResp.role of
            Just GimsConductor -> SP.BUS_CONDUCTOR
            _ -> SP.BUS_DRIVER
      person <-
        QP.findByEmailAndMerchantIdAndRole (Just email) merchant.id loginRole
          >>= maybe
            ( do
                basePerson <- makePerson req' transporterConfig mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice Nothing mbCloudType merchant.id merchantOpCityId False (Just loginRole)
                let conductorPerson = basePerson {SP.operatorBadgeToken = Just operatorBadgeToken}
                void $ QP.create conductorPerson
                createDriverDetails conductorPerson.id merchant.id merchantOpCityId transporterConfig
                pure conductorPerson
            )
            ( \existingPerson ->
                if existingPerson.operatorBadgeToken == Just operatorBadgeToken
                  then pure existingPerson
                  else do
                    let refreshedPerson = existingPerson {SP.operatorBadgeToken = Just operatorBadgeToken}
                    QP.updateByPrimaryKey refreshedPerson
                    pure refreshedPerson
            )
      checkSlidingWindowLimit (authHitsCountKey person)
      let entityId = getId person.id
          useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
          scfg = sessionConfig smsCfg
          mkId = getId merchant.id
      token <- makeSession scfg entityId mkId SR.USER useFakeOtpM merchantOpCityId.getId SR.SIGNATURE SR.DIRECT
      _ <- QR.create token
      void $ QP.updatePersonVersionsAndMerchantOperatingCity person mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice (Just $ deploymentVersion.getDeploymentVersion) merchantOpCityId mbCloudType
      cleanCachedTokens person.id
      QR.deleteByPersonIdExceptNew person.id token.id
      _ <- QR.setVerified True token.id
      when person.isNew $ QP.setIsNewFalse False person.id
      decPerson <- decrypt person
      let personAPIEntity = SP.makePersonAPIEntity decPerson
      return $ AuthRes token.id token.attempts (Just token.token) (Just personAPIEntity)
    _ -> do
      authRes <- authWithOtp isDashboard req' mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash mbXForwardedFor
      return $ AuthRes {attempts = authRes.attempts, authId = authRes.authId, token = Nothing, person = Nothing}

authWithOtp ::
  Bool ->
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow AuthWithOtpRes
authWithOtp isDashboard req' mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash mbXForwardedFor = do
  let req = if req'.merchantId == "2e8eac28-9854-4f5d-aea6-a2f6502cfe37" then req' {merchantId = "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f", merchantOperatingCity = Just (City.City "Kochi")} :: AuthReq else req' ---   "2e8eac28-9854-4f5d-aea6-a2f6502cfe37" -> YATRI_PARTNER_MERCHANT_ID  , "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f" -> NAMMA_YATRI_PARTNER_MERCHANT_ID
  deploymentVersion <- asks (.version)
  cloudType <- asks (.cloudType)
  runRequestValidation validateInitiateLoginReq req
  mbClientIP <- extractClientIP mbXForwardedFor
  let identifierType = fromMaybe SP.MOBILENUMBER req'.identifierType

  smsCfg <- asks (.smsCfg)
  let merchantId = Id req.merchantId :: Id DO.Merchant
  merchant <-
    QMerchant.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant req.merchantOperatingCity
  whenJust mbClientIP $ \clientIP -> do
    logInfo $ "Driver auth request from IP: " <> clientIP
    ipBlocked <- isIPBlocked merchantOpCityId.getId clientIP
    when ipBlocked $ throwError IpHitsLimitExceeded

  (person, otpChannel) <-
    case identifierType of
      SP.MOBILENUMBER -> do
        countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for mobileNumber auth")
        mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileNumber is required for mobileNumber auth")
        let otpChannel = fromMaybe SOTP.defaultOTPChannel req.otpChannel
        mobileNumberHash <- getDbHash mobileNumber
        person <-
          QP.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchant.id SP.DRIVER
            >>= maybe (createDriverWithDetails req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice (Just deploymentVersion.getDeploymentVersion) cloudType merchant.id merchantOpCityId isDashboard) return
        return (person, otpChannel)
      SP.EMAIL -> do
        email <- req.email & fromMaybeM (InvalidRequest "Email is required for email auth")
        person <-
          QP.findByEmailAndMerchantIdAndRole (Just email) merchant.id SP.DRIVER
            >>= maybe (createDriverWithDetails req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice (Just deploymentVersion.getDeploymentVersion) cloudType merchant.id merchantOpCityId isDashboard) return
        return (person, SOTP.EMAIL)
      SP.AADHAAR -> throwError $ InvalidRequest "Not implemented yet"
      SP.GIMS_EMAIL_PASSWORD -> throwError $ InvalidRequest "GIMS_EMAIL_PASSWORD does not use OTP auth"

  whenJust mbClientIP $ \clientIP -> fork "Driver Auth IP Fraud Check" $ do
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    checkAndUpdateAuthFraudByIP merchantOpCityId.getId clientIP transporterConfig
  checkSlidingWindowLimit (authHitsCountKey person)
  void $ cachePersonOTPChannel person.id otpChannel
  let entityId = getId $ person.id
      useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      scfg = sessionConfig smsCfg
  let mkId = getId merchantId
  token <- makeSession scfg entityId mkId SR.USER useFakeOtpM merchantOpCityId.getId (castChannelToMedium otpChannel) SR.OTP
  _ <- QR.create token
  QP.updatePersonVersionsAndMerchantOperatingCity person mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice (Just deploymentVersion.getDeploymentVersion) merchantOpCityId cloudType
  let otpCode = SR.authValueHash token
  whenNothing_ useFakeOtpM $ do
    SOTP.sendOTP
      otpChannel
      otpCode
      person.id
      person.merchantId
      merchantOpCityId
      req.mobileCountryCode
      req.mobileNumber
      req.email
      mbSenderHash
  let attempts = SR.attempts token
      authId = SR.id token
  return $ AuthWithOtpRes {attempts, authId, otpCode}
  where
    castChannelToMedium :: SOTP.OTPChannel -> SR.Medium
    castChannelToMedium SOTP.SMS = SR.SMS
    castChannelToMedium SOTP.EMAIL = SR.EMAIL
    castChannelToMedium SOTP.WHATSAPP = SR.WHATSAPP

createDriverDetails :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id SP.Person -> Id DO.Merchant -> Id DMOC.MerchantOperatingCity -> TC.TransporterConfig -> m ()
createDriverDetails personId merchantId merchantOpCityId transporterConfig = do
  now <- getCurrentTime
  let driverId = cast personId
  mbDriverLicense <- runInReplica $ QDL.findByDriverId driverId
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            adminId = Nothing,
            merchantId = Just merchantId,
            active = False,
            onRide = False,
            specialLocWarriorEnabledAt = Nothing,
            enabled = False,
            blocked = False,
            numOfLocks = 0,
            verified = False,
            subscribed = True,
            isPetModeEnabled = False,
            paymentPending = False,
            autoPayStatus = Nothing,
            referralCode = Nothing,
            referredByFleetOwnerId = Nothing,
            referredByOperatorId = Nothing,
            referredByDriverId = Nothing,
            totalReferred = Just 0,
            lastEnabledOn = Nothing,
            canDowngradeToSedan = transporterConfig.canDowngradeToSedan,
            canDowngradeToHatchback = transporterConfig.canDowngradeToHatchback,
            canDowngradeToTaxi = transporterConfig.canDowngradeToTaxi,
            canSwitchToRental = transporterConfig.canSwitchToRental,
            canSwitchToInterCity = transporterConfig.canSwitchToInterCity,
            enableForAirport = DriverInfo.ENABLED,
            airportBlockExpiryTime = Nothing,
            canSwitchToIntraCity = True,
            aadhaarVerified = False,
            blockedReason = Nothing,
            blockExpiryTime = Nothing,
            mode = Just DriverInfo.OFFLINE,
            payerVpa = Nothing,
            blockStateModifier = Nothing,
            enabledAt = Nothing,
            createdAt = now,
            updatedAt = now,
            compAadhaarImagePath = Nothing,
            availableUpiApps = Nothing,
            driverDob = (.driverDob) =<< mbDriverLicense,
            airConditionScore = Nothing,
            merchantOperatingCityId = Just merchantOpCityId,
            acRestrictionLiftCount = 0,
            acUsageRestrictionType = DriverInfo.NoRestriction,
            lastACStatusCheckedAt = Nothing,
            hasAdvanceBooking = False,
            tollRelatedIssueCount = Nothing,
            drunkAndDriveViolationCount = Nothing,
            extraFareMitigationFlag = Nothing,
            forwardBatchingEnabled = False,
            payoutVpa = Nothing,
            isInteroperable = False,
            payoutRegistrationOrderId = Nothing,
            latestScheduledPickup = Nothing,
            latestScheduledBooking = Nothing,
            payoutVpaStatus = Nothing,
            payoutVpaBankAccount = Nothing,
            payoutRegAmountRefunded = Nothing,
            dailyCancellationRateBlockingCooldown = Nothing,
            weeklyCancellationRateBlockingCooldown = Nothing,
            blockReasonFlag = Nothing,
            onRideTripCategory = Nothing,
            preferredPrimarySpecialLocId = Nothing,
            preferredSecondarySpecialLocIds = [],
            isSpecialLocWarrior = False,
            driverTripEndLocation = Nothing,
            issueBreachCooldownTimes = Nothing,
            hasRideStarted = Nothing,
            softBlockExpiryTime = Nothing,
            softBlockReasonFlag = Nothing,
            softBlockStiers = Nothing,
            isBlockedForReferralPayout = Nothing,
            onboardingVehicleCategory = Nothing,
            servicesEnabledForSubscription = [DEP.YATRI_SUBSCRIPTION],
            driverFlowStatus = Just DriverFlowStatus.OFFLINE,
            onlineDurationRefreshedAt = Just now,
            panNumber = Nothing,
            tdsRate = transporterConfig.taxConfig.defaultTdsRate,
            aadhaarNumber = Nothing,
            dlNumber = Nothing,
            maxPickupRadius = Nothing,
            tripDistanceMaxThreshold = Nothing,
            tripDistanceMinThreshold = Nothing,
            rideRequestVolume = Nothing,
            isSilentModeEnabled = Nothing,
            isTTSEnabled = Nothing,
            isHighAccuracyLocationEnabled = Nothing,
            rideRequestVolumeEnabled = Nothing,
            lastOfflineTime = Just now,
            ruleBasedUpgradeTiers = Nothing,
            dailyExtraKms = Nothing,
            weeklyExtraKms = Nothing,
            onboardingAs = Nothing,
            tollRouteBlockedTill = Nothing,
            approved = Nothing,
            docsVerificationStatus =
              if transporterConfig.enableManualDocumentStatusCheck == Just True
                then Just DDVS.ADMIN_PENDING
                else Nothing,
            address = Nothing,
            addressDocumentType = Nothing,
            nomineeName = Nothing,
            nomineeRelationship = Nothing,
            driverBankAccountDetails = Nothing,
            isBlockedForScheduledPayout = Nothing,
            disabledReasonFlag = Nothing
          }
  QDriverStats.createInitialDriverStats merchantOperatingCity.currency merchantOperatingCity.distanceUnit driverId
  QD.create driverInfo
  pure ()

makePerson ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  AuthReq ->
  TC.TransporterConfig ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe CloudType ->
  Id DO.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  Maybe SP.Role ->
  m SP.Person
makePerson req transporterConfig mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice mbBackendApp mbCloudType merchantId merchantOperatingCityId isDashboard mbRole = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  let identifierType = fromMaybe SP.MOBILENUMBER req.identifierType
  (email, encMobNum, useFakeOtp) <-
    case identifierType of
      SP.MOBILENUMBER -> do
        case (req.mobileNumber, req.mobileCountryCode) of
          (Just mobileNumber, Just _) -> do
            let useFakeOtp = if mobileNumber `elem` transporterConfig.fakeOtpMobileNumbers then Just "7891" else Nothing
            encMobNum <- encrypt mobileNumber
            return (Nothing, Just encMobNum, useFakeOtp)
          (_, _) -> throwError $ InvalidRequest "phone number and country code required"
      SP.EMAIL -> do
        case req.email of
          Just email -> do
            let useFakeOtp = if email `elem` transporterConfig.fakeOtpEmails then Just "7891" else Nothing
            pure (Just email, Nothing, useFakeOtp)
          Nothing -> throwError $ InvalidRequest "Email is required"
      SP.AADHAAR -> throwError $ InvalidRequest "Not implemented yet"
      SP.GIMS_EMAIL_PASSWORD -> do
        case req.email of
          Just email -> pure (Just email, Nothing, Nothing)
          Nothing -> throwError $ InvalidRequest "Email is required for GIMS_EMAIL_PASSWORD auth"
  safetyCohortNewTag <- Yudhishthira.fetchNammaTagExpiry (cast merchantOperatingCityId) $ LYT.TagNameValue "SafetyCohort#New"
  return $
    SP.Person
      { id = pid,
        firstName = fromMaybe "Driver" req.name,
        middleName = Nothing,
        lastName = Nothing,
        role = fromMaybe SP.DRIVER mbRole,
        gender = SP.UNKNOWN,
        hometown = Nothing,
        languagesSpoken = Nothing,
        identifierType = identifierType,
        email = email,
        passwordHash = Nothing,
        mobileNumber = encMobNum,
        mobileCountryCode = req.mobileCountryCode,
        identifier = Nothing,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        isNew = True,
        onboardedFromDashboard = isDashboard,
        deviceToken = Nothing,
        language = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now,
        clientBundleVersion = mbBundleVersion,
        clientSdkVersion = mbClientVersion,
        clientConfigVersion = mbClientConfigVersion,
        clientDevice = getDeviceFromText mbDevice,
        backendConfigVersion = Nothing,
        backendAppVersion = mbBackendApp,
        whatsappNotificationEnrollStatus = Nothing,
        alternateMobileNumber = Nothing,
        faceImageId = Nothing,
        qrImageId = Nothing,
        totalEarnedCoins = 0,
        usedCoins = 0,
        registrationLat = req.registrationLat,
        registrationLon = req.registrationLon,
        useFakeOtp,
        clientId = Nothing,
        driverTag = Just [safetyCohortNewTag],
        maskedMobileDigits = fmap (takeEnd 4) req.mobileNumber,
        nyClubConsent = Just False,
        reactBundleVersion = mbReactBundleVersion,
        cloudType = mbCloudType,
        operatorBadgeToken = Nothing
      }

makeSession ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r
  ) =>
  SmsSessionConfig ->
  Text ->
  Text ->
  SR.RTEntityType ->
  Maybe Text ->
  Text ->
  SR.Medium ->
  SR.LoginType ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId merchantId entityType fakeOtp merchantOpCityId authMedium authType = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- generateGUID
  token <- generateGUID
  altNumAttempts <- B.runInReplica $ QR.getAlternateNumberAttempts (Id entityId)
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = attempts,
        authMedium = authMedium,
        authType = authType,
        authValueHash = otp,
        verified = False,
        authExpiry = authExpiry,
        tokenExpiry = tokenExpiry,
        entityId = entityId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        entityType = entityType,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        alternateNumberAttempts = altNumAttempts
      }

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BPP:Registration:verify:" <> getId id <> ":hitsCount"

createDriverWithDetails ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe CloudType ->
  Id DO.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  m SP.Person
createDriverWithDetails req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice mbBackendApp mbCloudType merchantId merchantOpCityId isDashboard = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- makePerson req transporterConfig mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice mbBackendApp mbCloudType merchantId merchantOpCityId isDashboard Nothing
  void $ QP.create person
  createDriverDetails (person.id) merchantId merchantOpCityId transporterConfig
  pure person

verify ::
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id SR.RegistrationToken ->
  AuthVerifyReq ->
  Maybe Text ->
  m AuthVerifyRes
verify tokenId req mbXForwardedFor = do
  runRequestValidation validateAuthVerifyReq req
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId
  fork "generating the referral code for driver" $ do
    void $ generateReferralCode (Just person.role) (person.id, person.merchantId, Id merchantOperatingCityId)

  let isNewPerson = person.isNew
  let deviceToken = Just req.deviceToken
  when (isNothing person.deviceToken) $ do
    TM.notifyMarketingEvents person.id deviceToken TM.NEW_SIGNUP Nothing (TM.MerchantOperatingCityId (Id merchantOperatingCityId)) [TM.FIREBASE]
  cleanCachedTokens person.id
  QR.deleteByPersonIdExceptNew person.id tokenId
  _ <- QR.setVerified True tokenId
  fork "Decrement Auth IP Counter" $ do
    mbClientIP <- extractClientIP mbXForwardedFor
    whenJust mbClientIP $ \clientIP -> do
      mbTransporterConfig <- SCTC.findByMerchantOpCityId (Id merchantOperatingCityId) Nothing
      whenJust mbTransporterConfig $ decrementAuthCountersByIP merchantOperatingCityId clientIP
  _ <- QP.updateDeviceToken deviceToken person.id
  when isNewPerson $
    QP.setIsNewFalse False person.id
  updPers <- QP.findById (Id entityId) >>= fromMaybeM (PersonNotFound entityId)
  ensureFleetEnabledForDriver updPers
  decPerson <- decrypt updPers
  unless (decPerson.whatsappNotificationEnrollStatus == req.whatsappNotificationEnroll && isJust req.whatsappNotificationEnroll) $ do
    fork "whatsapp_opt_api_call" $ do
      case decPerson.mobileNumber of
        Nothing -> throwError $ AuthBlocked "Mobile Number is null"
        Just mobileNo -> callWhatsappOptApi mobileNo person.id req.whatsappNotificationEnroll person.merchantId (Id merchantOperatingCityId)
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

-- | Block driver login when their fleet has been disabled. Also self-heals
--   by setting the FleetDisabled flag if the cascade missed this driver.
ensureFleetEnabledForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => SP.Person -> m ()
ensureFleetEnabledForDriver person =
  when (person.role == SP.DRIVER) $ do
    mbAssoc <- QFDA.findByDriverId person.id True
    whenJust mbAssoc $ \assoc -> do
      mbFleet <- QFOI.findByPrimaryKey (Id assoc.fleetOwnerId)
      whenJust mbFleet $ \fleet ->
        unless fleet.enabled $ do
          QDIExtra.markDisabledForFleetCascade (cast person.id)
          throwError $ InvalidRequest "Your Fleet has been disabled"

callWhatsappOptApi ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasKafkaProducer r
  ) =>
  Text ->
  Id SP.Person ->
  Maybe Whatsapp.OptApiMethods ->
  Id DO.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
callWhatsappOptApi mobileNo personId hasOptedIn merchantId merchantOpCityId = do
  let status = fromMaybe Whatsapp.OPT_IN hasOptedIn
  void $ Whatsapp.whatsAppOptAPI merchantId merchantOpCityId $ Whatsapp.OptApiReq {phoneNumber = mobileNo, method = status}
  QP.updateWhatsappNotificationEnrollStatus (Just status) personId

checkRegistrationTokenExists :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id SR.RegistrationToken -> m SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

checkPersonExists :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m SP.Person
checkPersonExists entityId =
  QP.findById (Id entityId) >>= fromMaybeM (PersonNotFound entityId)

resend ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
    HasField "emailServiceConfig" r Email.Types.EmailServiceConfig,
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasKafkaProducer r
  ) =>
  Id SR.RegistrationToken ->
  Maybe Text ->
  m ResendAuthRes
resend tokenId mbSenderHash = do
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  let merchantOpCityId = Id merchantOperatingCityId
  let otpCode = authValueHash
  otpChannel <- getPersonOTPChannel person.id
  mobileNumber <- mapM decrypt person.mobileNumber
  let receiverEmail = person.email

  SOTP.sendOTP
    otpChannel
    otpCode
    person.id
    person.merchantId
    merchantOpCityId
    person.mobileCountryCode
    mobileNumber
    receiverEmail
    mbSenderHash

  void $ QR.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1) Nothing Nothing

cleanCachedTokens :: (EsqDBFlow m r, Redis.HedisFlow m r, CacheFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId.getId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

authHitOTPChannel :: Id SP.Person -> Text
authHitOTPChannel personId = "BPP:Registration:auth" <> getId personId <> ":OtpChannel"

cachePersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> SOTP.OTPChannel -> m ()
cachePersonOTPChannel personId otpChannel = do
  Redis.setExp (authHitOTPChannel personId) otpChannel 1800

getPersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> m SOTP.OTPChannel
getPersonOTPChannel personId = do
  Redis.get (authHitOTPChannel personId) >>= \case
    Just a -> pure a
    Nothing -> pure SOTP.SMS

logout ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    Redis.HedisLTSFlowEnv r
  ) =>
  (Id SP.Person, Id DO.Merchant, Id DMOC.MerchantOperatingCity) ->
  m APISuccess
logout (personId, _, merchantOpCityId) = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  cleanCachedTokens personId
  uperson <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  _ <- QP.updateDeviceToken Nothing uperson.id
  QR.deleteByPersonId personId.getId
  when (uperson.role == SP.DRIVER) $ do
    driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
    let newFlowStatus = DDriverMode.getDriverFlowStatus (Just DriverInfo.OFFLINE) False
    DDriverMode.updateDriverModeAndFlowStatus uperson.id transporterConfig False (Just DriverInfo.OFFLINE) newFlowStatus driverInfo Nothing Nothing
  pure Success

marketingEventsPreLogin ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EventStreamFlow m r
  ) =>
  MarketEventReq ->
  m APISuccess
marketingEventsPreLogin req = do
  let params = req.marketingParams
  now <- getCurrentTime
  let marketingParamsData =
        TE.MarketingParamsEventPreLoginData
          { gclId = params.gclId,
            utmCampaign = params.utmCampaign,
            utmContent = params.utmContent,
            utmCreativeFormat = params.utmCreativeFormat,
            utmMedium = params.utmMedium,
            utmSource = params.utmSource,
            utmTerm = params.utmTerm,
            appName = params.appName,
            userType = params.userType,
            createdAt = now,
            updatedAt = now
          }
  TE.triggerMarketingParamEventPreLogin marketingParamsData
  pure Success

marketingEventsPostLogin ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EventStreamFlow m r
  ) =>
  (Id SP.Person, Id DO.Merchant, Id DMOC.MerchantOperatingCity) ->
  MarketEventReq ->
  m APISuccess
marketingEventsPostLogin (personId, merchantId, merchantOpCityId) req = do
  let params = req.marketingParams
  now <- getCurrentTime
  let marketingParamsData =
        TE.MarketingParamsEventData
          { personId = personId,
            gclId = params.gclId,
            utmCampaign = params.utmCampaign,
            utmContent = params.utmContent,
            utmCreativeFormat = params.utmCreativeFormat,
            utmMedium = params.utmMedium,
            utmSource = params.utmSource,
            utmTerm = params.utmTerm,
            appName = params.appName,
            userType = params.userType,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
  TE.triggerMarketingParamEvent marketingParamsData
  pure Success

signatureAuth ::
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow AuthRes
signatureAuth req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice mbClientId = do
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for signature auth")
  mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileNumber is required for signature auth")
  deploymentVersion <- asks (.version)
  cloudType <- asks (.cloudType)
  merchant <-
    QMerchant.findById (Id req.merchantId)
      >>= fromMaybeM (MerchantNotFound (req.merchantId))
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant req.merchantOperatingCity
  mobileNumberDecrypted <- decryptAES128 merchant.cipherText mobileNumber
  let reqWithMobileNumber = req {mobileNumber = Just mobileNumberDecrypted}
  mobileNumberHash <- getDbHash mobileNumberDecrypted
  person <-
    QP.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchant.id SP.DRIVER
      >>= maybe (createDriverWithDetails reqWithMobileNumber mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice (Just $ deploymentVersion.getDeploymentVersion) cloudType merchant.id merchantOpCityId False) return -- Simple fallback for create, refining
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      scfg = sessionConfig smsCfg
      mkId = getId merchant.id
  -- Authentication flow
  token <- makeSession scfg entityId mkId SR.USER useFakeOtpM merchantOpCityId.getId SR.SIGNATURE SR.DIRECT
  _ <- QR.create token
  void $ QP.updatePersonVersionsAndMerchantOperatingCity person mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (Just $ deploymentVersion.getDeploymentVersion) merchantOpCityId cloudType
  -- Verification flow
  fork "generating the referral code for driver" $ do
    void $ generateReferralCode (Just person.role) (person.id, person.merchantId, merchantOpCityId)
  cleanCachedTokens person.id
  QR.deleteByPersonIdExceptNew person.id token.id
  let isNewPerson = person.isNew
  _ <- QR.setVerified True token.id
  when isNewPerson $
    QP.setIsNewFalse False person.id
  decPerson <- decrypt person
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ AuthRes token.id token.attempts (Just token.token) (Just personAPIEntity)

mkAuthIPCounterKey :: Text -> Text -> Text
mkAuthIPCounterKey mocId clientIP = "Driver:AuthCount:" <> clientIP <> ":" <> mocId

mkIPBlockKey :: Text -> Text -> Text
mkIPBlockKey mocId clientIP = "Driver:IPBlocked:" <> clientIP <> ":" <> mocId

lookupClientIPPositionFromRight :: IO Int
lookupClientIPPositionFromRight = fromMaybe 3 . (>>= readMaybe) <$> SE.lookupEnv "XFF_CLIENT_IP_POSITION_FROM_RIGHT"

extractClientIP :: MonadIO m => Maybe Text -> m (Maybe Text)
extractClientIP mbXForwardedFor = do
  clientIPPosition <- liftIO lookupClientIPPositionFromRight
  pure $
    mbXForwardedFor
      >>= \headerValue ->
        let ips = mapMaybe (\entry -> let s = T.strip entry in if T.null s then Nothing else Just s) (T.splitOn "," headerValue)
            idx = List.length ips - clientIPPosition
         in (if idx >= 0 then listToMaybe (List.drop idx ips) else Nothing) >>= \clientIP ->
              let ipWithoutPort = T.takeWhile (/= ':') clientIP
               in if T.null ipWithoutPort then Nothing else Just ipWithoutPort

isIPBlocked :: (CacheFlow m r, MonadFlow m) => Text -> Text -> m Bool
isIPBlocked mocId clientIP = Redis.withNonCriticalCrossAppRedis $ do
  blockResult <- Redis.get (mkIPBlockKey mocId clientIP)
  pure $ isJust (blockResult :: Maybe Text)

checkAndUpdateAuthFraudByIP :: (CacheFlow m r, MonadFlow m) => Text -> Text -> TC.TransporterConfig -> m ()
checkAndUpdateAuthFraudByIP mocId clientIP transporterConfig = Redis.withNonCriticalCrossAppRedis $
  whenJust transporterConfig.fraudAuthCountWindow $ \window -> do
    SWC.incrementWindowCount (mkAuthIPCounterKey mocId clientIP) window
    whenJust transporterConfig.fraudAuthCountThreshold $ \threshold -> do
      authCount <- SWC.getCurrentWindowCount (mkAuthIPCounterKey mocId clientIP) window
      when (authCount >= fromIntegral threshold) $ do
        logInfo $ "Driver auth fraud detected for IP " <> clientIP <> " with count " <> show authCount
        SWC.deleteCurrentWindowValues (mkAuthIPCounterKey mocId clientIP) window
        whenJust transporterConfig.authIpBlockedUntilInMins $ \mins -> do
          let ttlSeconds = fromIntegral mins * 60
          void $ Redis.setExp (mkIPBlockKey mocId clientIP) ("true" :: Text) ttlSeconds
          logInfo $ "Driver auth IP " <> clientIP <> " blocked for " <> show mins <> " minutes in city " <> mocId

decrementAuthCountersByIP :: (CacheFlow m r, MonadFlow m) => Text -> Text -> TC.TransporterConfig -> m ()
decrementAuthCountersByIP mocId clientIP transporterConfig =
  Redis.withNonCriticalCrossAppRedis $
    whenJust transporterConfig.fraudAuthCountWindow $
      SWC.decrementWindowCount (mkAuthIPCounterKey mocId clientIP)
