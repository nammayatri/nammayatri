{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Registration
  ( AuthReq (..),
    AuthRes (..),
    CreatePersonInput (..),
    ResendAuthRes,
    AuthVerifyReq (..),
    AuthVerifyRes (..),
    auth,
    verify,
    resend,
    logout,
    cleanCachedTokens,
    createDriverWithDetails,
  )
where

import Data.OpenApi hiding (email, info, url)
import Domain.Action.UI.DriverReferral
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Merchant as DO
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.TransporterConfig as TC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common as BC
import Kernel.Types.Id
import qualified Kernel.Types.Predicate as P
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import Kernel.Utils.Version
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.DriverInformation as QD
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth (authTokenCacheKey)
import Tools.Error
import Tools.SMS as Sms hiding (Success)
import Tools.Whatsapp as Whatsapp

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: Text,
    merchantOperatingCity :: Maybe Context.City,
    registrationLat :: Maybe Double,
    registrationLon :: Maybe Double
  }
  deriving (Generic, FromJSON, ToSchema)

data CreatePersonInput = CreatePersonInput
  { mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCity :: Maybe Context.City,
    email :: Maybe Text,
    identifierType :: SP.IdentifierType,
    registrationLat :: Maybe Double,
    registrationLon :: Maybe Double
  }

validateInitiateLoginReq :: Validate AuthReq
validateInitiateLoginReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data AuthRes = AuthRes
  { authId :: Id SR.RegistrationToken,
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

authHitsCountKey :: SP.Person -> Text
authHitsCountKey person = "BPP:Registration:auth:" <> getId person.id <> ":hitsCount"

auth ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion],
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  Bool ->
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  m AuthRes
auth isDashboard req' mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice = do
  let req = if req'.merchantId == "2e8eac28-9854-4f5d-aea6-a2f6502cfe37" then req' {merchantId = "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f", merchantOperatingCity = Just City.Kochi} :: AuthReq else req' ---   "2e8eac28-9854-4f5d-aea6-a2f6502cfe37" -> YATRI_PARTNER_MERCHANT_ID  , "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f" -> NAMMA_YATRI_PARTNER_MERCHANT_ID
  deploymentVersion <- asks (.version)
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  mobileNumberHash <- getDbHash mobileNumber
  let merchantId = Id req.merchantId :: Id DO.Merchant
  merchant <-
    QMerchant.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant req.merchantOperatingCity
  let createDriverInput = buildCreatePersonInput req
  person <-
    QP.findByMobileNumberAndMerchant countryCode mobileNumberHash merchant.id
      >>= maybe (createDriverWithDetails createDriverInput mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice (Just deploymentVersion.getDeploymentVersion) merchant.id merchantOpCityId isDashboard) return
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp
      scfg = sessionConfig smsCfg
  let mkId = getId merchantId
  token <- makeSession scfg entityId mkId SR.USER useFakeOtpM merchantOpCityId.getId
  _ <- QR.create token
  QP.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice (Just deploymentVersion.getDeploymentVersion)
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let otpCode = SR.authValueHash token
        phoneNumber = countryCode <> mobileNumber
        sender = smsCfg.sender
    withLogTag ("personId_" <> getId person.id) $ do
      message <-
        MessageBuilder.buildSendOTPMessage merchantOpCityId $
          MessageBuilder.BuildSendOTPMessageReq
            { otp = otpCode,
              hash = otpHash
            }
      Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  let attempts = SR.attempts token
      authId = SR.id token
  return $ AuthRes {attempts, authId}
  where
    buildCreatePersonInput AuthReq {..} = CreatePersonInput {mobileNumber = Just mobileNumber, mobileCountryCode = Just mobileCountryCode, email = Nothing, identifierType = SP.MOBILENUMBER, ..}

createDriverDetails :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id SP.Person -> Id DO.Merchant -> Id DMOC.MerchantOperatingCity -> TC.TransporterConfig -> m ()
createDriverDetails personId merchantId merchantOpCityId transporterConfig = do
  now <- getCurrentTime
  let driverId = cast personId
  mbDriverLicense <- runInReplica $ QDL.findByDriverId driverId
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            adminId = Nothing,
            merchantId = Just merchantId,
            active = False,
            onRide = False,
            enabled = False,
            blocked = False,
            numOfLocks = 0,
            verified = False,
            subscribed = True,
            paymentPending = False,
            autoPayStatus = Nothing,
            referralCode = Nothing,
            referredByDriverId = Nothing,
            totalReferred = Just 0,
            lastEnabledOn = Nothing,
            canDowngradeToSedan = transporterConfig.canDowngradeToSedan,
            canDowngradeToHatchback = transporterConfig.canDowngradeToHatchback,
            canDowngradeToTaxi = transporterConfig.canDowngradeToTaxi,
            canSwitchToRental = transporterConfig.canSwitchToRental,
            canSwitchToInterCity = transporterConfig.canSwitchToInterCity,
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
            lastACStatusCheckedAt = Nothing
          }
  QDriverStats.createInitialDriverStats driverId
  QD.create driverInfo
  pure ()

makePerson :: EncFlow m r => CreatePersonInput -> TC.TransporterConfig -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Id DO.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> m SP.Person
makePerson req transporterConfig mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice mbBackendApp merchantId merchantOperatingCityId isDashboard = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  (email, encMobNum, useFakeOtp) <-
    case req.identifierType of
      SP.MOBILENUMBER -> do
        case (req.mobileNumber, req.mobileCountryCode) of
          (Just mn, Just _) -> do
            let useFakeOtp = if mn `elem` transporterConfig.fakeOtpMobileNumbers then Just "7891" else Nothing
            encMobNum <- encrypt mn
            return (Nothing, Just encMobNum, useFakeOtp)
          (_, _) -> throwError $ InvalidRequest "phone number and country code required"
      SP.EMAIL -> do
        case req.email of
          Just email -> pure (Just email, Nothing, Nothing)
          Nothing -> throwError $ InvalidRequest "Email is required"
      SP.AADHAAR -> throwError $ InvalidRequest "Not implemented yet"
  return $
    SP.Person
      { id = pid,
        firstName = "Driver",
        middleName = Nothing,
        lastName = Nothing,
        role = SP.DRIVER,
        gender = SP.UNKNOWN,
        hometown = Nothing,
        languagesSpoken = Nothing,
        identifierType = req.identifierType,
        email = email,
        passwordHash = Nothing,
        unencryptedMobileNumber = req.mobileNumber,
        mobileNumber = encMobNum,
        mobileCountryCode = req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
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
        unencryptedAlternateMobileNumber = Nothing,
        alternateMobileNumber = Nothing,
        faceImageId = Nothing,
        totalEarnedCoins = 0,
        usedCoins = 0,
        registrationLat = req.registrationLat,
        registrationLon = req.registrationLon,
        useFakeOtp,
        hasCompletedSafetySetup = Nothing
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
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId merchantId entityType fakeOtp merchantOpCityId = do
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
        authMedium = SR.SMS,
        authType = SR.OTP,
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

createDriverWithDetails :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => CreatePersonInput -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Id DO.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> m SP.Person
createDriverWithDetails req mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice mbBackendApp merchantId merchantOpCityId isDashboard = do
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOpCityId Nothing Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- makePerson req transporterConfig mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice mbBackendApp merchantId merchantOpCityId isDashboard
  void $ QP.create person
  createDriverDetails (person.id) merchantId merchantOpCityId transporterConfig
  pure person

verify ::
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id SR.RegistrationToken ->
  AuthVerifyReq ->
  m AuthVerifyRes
verify tokenId req = do
  runRequestValidation validateAuthVerifyReq req
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId
  fork "generating the referral code for driver" $ do
    void $ generateReferralCode (person.id, person.merchantId, Id merchantOperatingCityId)

  let isNewPerson = person.isNew
  let deviceToken = Just req.deviceToken
  cleanCachedTokens person.id
  QR.deleteByPersonIdExceptNew person.id tokenId
  _ <- QR.setVerified tokenId
  _ <- QP.updateDeviceToken person.id deviceToken
  when isNewPerson $
    QP.setIsNewFalse person.id
  updPers <- QP.findById (Id entityId) >>= fromMaybeM (PersonNotFound entityId)
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

callWhatsappOptApi ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
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
  QP.updateWhatsappNotificationEnrollStatus personId $ Just status

checkRegistrationTokenExists :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id SR.RegistrationToken -> m SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

checkPersonExists :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m SP.Person
checkPersonExists entityId =
  QP.findById (Id entityId) >>= fromMaybeM (PersonNotFound entityId)

resend ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  Id SR.RegistrationToken ->
  m ResendAuthRes
resend tokenId = do
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let otpCode = authValueHash
  let otpHash = smsCfg.credConfig.otpHash
      phoneNumber = countryCode <> mobileNumber
      sender = smsCfg.sender
  withLogTag ("personId_" <> entityId) $ do
    message <-
      MessageBuilder.buildSendOTPMessage (Id merchantOperatingCityId) $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otpCode,
            hash = otpHash
          }
    Sms.sendSMS person.merchantId (Id merchantOperatingCityId) (Sms.SendSMSReq message phoneNumber sender)
      >>= Sms.checkSmsResult
  _ <- QR.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1)

cleanCachedTokens :: (EsqDBFlow m r, Redis.HedisFlow m r, CacheFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

logout ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  (Id SP.Person, Id DO.Merchant, Id DMOC.MerchantOperatingCity) ->
  m APISuccess
logout (personId, _, _) = do
  cleanCachedTokens personId
  uperson <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  _ <- QP.updateDeviceToken uperson.id Nothing
  QR.deleteByPersonId personId
  when (uperson.role == SP.DRIVER) $ void (QD.updateActivity False (Just DriverInfo.OFFLINE) (cast uperson.id))
  pure Success
