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
    ResendAuthRes,
    AuthVerifyReq (..),
    AuthVerifyRes (..),
    OTPChannel (..),
    auth,
    signatureAuth,
    verify,
    resend,
    logout,
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types ((.:), (.:?))
import Data.OpenApi hiding (email, info)
import qualified Data.Text.Encoding as TE
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person (PersonAPIEntity, PersonE (updatedAt))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Person.PersonStats as DPS
import Domain.Types.RegistrationToken (RegistrationToken)
import qualified Domain.Types.RegistrationToken as SR
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Types as Language
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.APISuccess as AP
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Common as BC
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QDFS
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Person.PersonDisability as PDisability
import qualified Storage.Queries.Person.PersonStats as QPS
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Auth (authTokenCacheKey, decryptAES128)
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.SMS as Sms
import Tools.Whatsapp
import qualified Tools.Whatsapp as Whatsapp

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: ShortId Merchant,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe SP.Gender,
    otpChannel :: Maybe OTPChannel
  }
  deriving (Generic, ToJSON, Show, ToSchema)

instance A.FromJSON AuthReq where
  parseJSON v = case v of
    A.Object obj ->
      AuthReq <$> obj .: "mobileNumber"
        <*> obj .: "mobileCountryCode"
        <*> obj .: "merchantId"
        <*> obj .:? "deviceToken"
        <*> obj .:? "userId" -- TODO :: This needs to be changed to notificationToken
        <*> obj .:? "whatsappNotificationEnroll"
        <*> obj .:? "firstName"
        <*> obj .:? "middleName"
        <*> obj .:? "lastName"
        <*> obj .:? "email"
        <*> obj .:? "language"
        <*> obj .:? "gender"
        <*> obj .:? "otpChannel"
    A.String s ->
      case A.eitherDecodeStrict (TE.encodeUtf8 s) of
        Left err -> fail err
        Right req -> return req
    _ -> fail "Invalid JSON format for AuthReq"

validateAuthReq :: Validate AuthReq
validateAuthReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

validateSignatureAuthReq :: Validate AuthReq
validateSignatureAuthReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data AuthRes = AuthRes
  { authId :: Id RegistrationToken,
    attempts :: Int,
    authType :: SR.LoginType,
    token :: Maybe Text,
    person :: Maybe PersonAPIEntity,
    isPersonBlocked :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- Need to have discussion around this
data OTPChannel = SMS | WHATSAPP
  deriving (Generic, Show, Enum, Eq, FromJSON, ToJSON, ToSchema)

defaultOTPChannel :: OTPChannel
defaultOTPChannel = SMS

type ResendAuthRes = AuthRes

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: Text,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

validateAuthVerifyReq :: Validate AuthVerifyReq
validateAuthVerifyReq AuthVerifyReq {..} =
  sequenceA_
    [ validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

data AuthVerifyRes = AuthVerifyRes
  { token :: Text,
    person :: PersonAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

authHitsCountKey :: SP.Person -> Text
authHitsCountKey person = "BAP:Registration:auth" <> getId person.id <> ":hitsCount"

auth ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    DB.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  m AuthRes
auth req mbBundleVersion mbClientVersion = do
  runRequestValidation validateAuthReq req
  smsCfg <- asks (.smsCfg)
  let countryCode = req.mobileCountryCode
      mobileNumber = req.mobileNumber
      notificationToken = req.notificationToken
      otpChannel = fromMaybe defaultOTPChannel req.otpChannel
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantNotFound $ getShortId req.merchantId)
  mobileNumberHash <- getDbHash mobileNumber
  person <-
    Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash merchant.id
      >>= maybe (createPerson req mobileNumber notificationToken mbBundleVersion mbClientVersion merchant) return
  let merchantOperatingCityId = person.merchantOperatingCityId
  checkSlidingWindowLimit (authHitsCountKey person)
  _ <- cachePersonOTPChannel person.id otpChannel
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  let mkId = getId $ merchant.id
  regToken <- makeSession scfg entityId mkId (show <$> useFakeOtpM)

  if not person.blocked
    then do
      void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
      _ <- RegistrationToken.create regToken
      when (isNothing useFakeOtpM) $ do
        let otpCode = SR.authValueHash regToken
        let otpHash = smsCfg.credConfig.otpHash
            phoneNumber = countryCode <> mobileNumber
            sender = smsCfg.sender
        case otpChannel of
          SMS ->
            withLogTag ("personId_" <> getId person.id) $ do
              message <-
                MessageBuilder.buildSendOTPMessage merchantOperatingCityId $
                  MessageBuilder.BuildSendOTPMessageReq
                    { otp = otpCode,
                      hash = otpHash
                    }
              Sms.sendSMS person.merchantId merchantOperatingCityId (Sms.SendSMSReq message phoneNumber sender)
                >>= Sms.checkSmsResult
          WHATSAPP ->
            withLogTag ("personId_" <> getId person.id) $ do
              _ <- callWhatsappOptApi phoneNumber person.id merchant.id (Just Whatsapp.OPT_IN)
              result <- Whatsapp.whatsAppOtpApi person.merchantId merchantOperatingCityId (Whatsapp.SendOtpApiReq phoneNumber otpCode)
              when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp OTP message")
    else logInfo $ "Person " <> getId person.id <> " is not enabled. Skipping send OTP"
  return $ AuthRes regToken.id regToken.attempts regToken.authType Nothing Nothing (Just person.blocked)

signatureAuth ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    CacheFlow m r,
    DB.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  m AuthRes
signatureAuth req mbBundleVersion mbClientVersion = do
  runRequestValidation validateSignatureAuthReq req
  smsCfg <- asks (.smsCfg)
  let countryCode = req.mobileCountryCode
      deviceToken = req.deviceToken
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantNotFound $ getShortId req.merchantId)
  mobileNumber <- decryptAES128 merchant.cipherText req.mobileNumber
  notificationToken <- mapM (decryptAES128 merchant.cipherText) req.notificationToken
  mobileNumberHash <- getDbHash mobileNumber
  person <-
    Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash merchant.id
      >>= maybe (createPerson req mobileNumber notificationToken mbBundleVersion mbClientVersion merchant) return
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  let mkId = getId $ merchant.id
  regToken <- makeSession scfg entityId mkId (show <$> useFakeOtpM)
  if not person.blocked
    then do
      void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
      _ <- RegistrationToken.create regToken
      mbEncEmail <- encrypt `mapM` req.email
      _ <- RegistrationToken.setDirectAuth regToken.id
      _ <- Person.updatePersonalInfo person.id (req.firstName <|> person.firstName <|> Just "User") req.middleName req.lastName Nothing mbEncEmail deviceToken notificationToken (req.language <|> person.language <|> Just Language.ENGLISH) (req.gender <|> Just person.gender) (mbClientVersion <|> Nothing) (mbBundleVersion <|> Nothing)
      personAPIEntity <- verifyFlow person regToken req.whatsappNotificationEnroll deviceToken
      return $ AuthRes regToken.id regToken.attempts SR.DIRECT (Just regToken.token) (Just personAPIEntity) (Just person.blocked)
    else return $ AuthRes regToken.id regToken.attempts regToken.authType Nothing Nothing (Just person.blocked)

buildPerson :: (EncFlow m r, DB.EsqDBReplicaFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, CacheFlow m r) => AuthReq -> Text -> Maybe Text -> Maybe Version -> Maybe Version -> Id DMerchant.Merchant -> Context.City -> Id DMOC.MerchantOperatingCity -> m SP.Person
buildPerson req mobileNumber notificationToken bundleVersion clientVersion merchantId currentCity merchantOperatingCityId = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  personWithSameDeviceToken <- listToMaybe <$> runInReplica (Person.findBlockedByDeviceToken req.deviceToken)
  let isBlockedBySameDeviceToken = maybe False (.blocked) personWithSameDeviceToken
  useFraudDetection <- do
    if isBlockedBySameDeviceToken
      then do
        merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
        return merchantConfig.useFraudDetection
      else return False
  encMobNum <- encrypt mobileNumber
  encEmail <- mapM encrypt req.email
  return $
    SP.Person
      { id = pid,
        firstName = req.firstName,
        middleName = req.middleName,
        lastName = req.lastName,
        role = SP.USER,
        gender = fromMaybe SP.UNKNOWN req.gender,
        identifierType = SP.MOBILENUMBER,
        email = encEmail,
        passwordHash = Nothing,
        unencryptedMobileNumber = Just mobileNumber,
        mobileNumber = Just encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        totalRatings = 0,
        totalRatingScore = 0,
        isValidRating = False,
        language = req.language,
        isNew = True,
        enabled = not (useFraudDetection && isBlockedBySameDeviceToken),
        blocked = useFraudDetection && isBlockedBySameDeviceToken,
        deviceToken = req.deviceToken,
        notificationToken = notificationToken,
        description = Nothing,
        merchantId = merchantId,
        currentCity = currentCity,
        merchantOperatingCityId = merchantOperatingCityId,
        referralCode = Nothing,
        referredAt = Nothing,
        hasTakenValidRide = False,
        hasDisability = Nothing,
        createdAt = now,
        updatedAt = now,
        blockedAt = if useFraudDetection then personWithSameDeviceToken >>= (.blockedAt) else Nothing,
        blockedByRuleId = if useFraudDetection then personWithSameDeviceToken >>= (.blockedByRuleId) else Nothing,
        aadhaarVerified = False,
        bundleVersion = bundleVersion,
        clientVersion = clientVersion,
        whatsappNotificationEnrollStatus = Nothing
      }

-- FIXME Why do we need to store always the same authExpiry and tokenExpiry from config? info field is always Nothing
makeSession ::
  MonadFlow m =>
  SmsSessionConfig ->
  Text ->
  Text ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId merchantId fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
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
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BAP:Registration:verify:" <> getId id <> ":hitsCount"

verifyFlow :: (EsqDBFlow m r, EncFlow m r, CacheFlow m r, MonadFlow m) => SP.Person -> SR.RegistrationToken -> Maybe Whatsapp.OptApiMethods -> Maybe Text -> m PersonAPIEntity
verifyFlow person regToken whatsappNotificationEnroll deviceToken = do
  let isNewPerson = person.isNew
  RegistrationToken.deleteByPersonIdExceptNew person.id regToken.id
  when isNewPerson $ Person.setIsNewFalse person.id
  when isNewPerson $
    Notify.notifyOnRegistration regToken person deviceToken
  updPerson <- Person.findById (Id regToken.entityId) >>= fromMaybeM (PersonDoesNotExist regToken.entityId)
  decPerson <- decrypt updPerson
  customerDisability <- B.runInReplica $ PDisability.findByPersonId person.id
  let tag = customerDisability <&> (.tag)
  let personAPIEntity = SP.makePersonAPIEntity decPerson tag
  unless (decPerson.whatsappNotificationEnrollStatus == whatsappNotificationEnroll && isJust whatsappNotificationEnroll) $ do
    fork "whatsapp_opt_api_call" $ do
      case decPerson.mobileNumber of
        Nothing -> throwError $ AuthBlocked "Mobile Number is null"
        Just mobileNo -> callWhatsappOptApi mobileNo person.id person.merchantId whatsappNotificationEnroll
  return personAPIEntity

verify ::
  ( CacheFlow m r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r
  ) =>
  Id SR.RegistrationToken ->
  AuthVerifyReq ->
  m AuthVerifyRes
verify tokenId req = do
  runRequestValidation validateAuthVerifyReq req
  regToken@SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId
  let merchantOperatingCityId = person.merchantOperatingCityId
  let deviceToken = Just req.deviceToken
  personWithSameDeviceToken <- listToMaybe <$> runInReplica (Person.findBlockedByDeviceToken deviceToken)
  let isBlockedBySameDeviceToken = maybe False (.blocked) personWithSameDeviceToken
  cleanCachedTokens person.id
  when isBlockedBySameDeviceToken $ do
    merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
    when merchantConfig.useFraudDetection $ SMC.blockCustomer person.id ((.blockedByRuleId) =<< personWithSameDeviceToken)
  void $ RegistrationToken.setVerified tokenId
  void $ Person.updateDeviceToken person.id deviceToken
  personAPIEntity <- verifyFlow person regToken req.whatsappNotificationEnroll deviceToken
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

callWhatsappOptApi ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  Text ->
  Id SP.Person ->
  Id DMerchant.Merchant ->
  Maybe Whatsapp.OptApiMethods ->
  m ()
callWhatsappOptApi mobileNo personId merchantId hasOptedIn = do
  let status = fromMaybe Whatsapp.OPT_IN hasOptedIn
  merchantOperatingCityId <- CQP.findCityInfoById personId >>= fmap (.merchantOperatingCityId) . fromMaybeM (PersonCityInformationDoesNotExist $ "personId:- " <> personId.getId)
  void $ whatsAppOptAPI merchantId merchantOperatingCityId $ Whatsapp.OptApiReq {phoneNumber = mobileNo, method = status}
  void $ Person.updateWhatsappNotificationEnrollStatus personId $ Just status

getRegistrationTokenE :: (CacheFlow m r, EsqDBFlow m r) => Id SR.RegistrationToken -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

createPerson ::
  (EncFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, Redis.HedisFlow m r, CacheFlow m r) => AuthReq -> Text -> Maybe Text -> Maybe Version -> Maybe Version -> DMerchant.Merchant -> m SP.Person
createPerson req mobileNumber notificationToken mbBundleVersion mbClientVersion merchant = do
  let currentCity = merchant.defaultCity
  merchantOperatingCityId <-
    CQMOC.findByMerchantIdAndCity merchant.id currentCity
      >>= fmap (.id)
        . fromMaybeM
          ( MerchantOperatingCityNotFound $
              "merchantId: " <> merchant.id.getId <> " ,city: " <> show currentCity
          )
  person <- buildPerson req mobileNumber notificationToken mbBundleVersion mbClientVersion merchant.id currentCity merchantOperatingCityId
  createPersonStats <- makePersonStats person
  _ <- Person.create person
  _ <- QDFS.create $ makeIdlePersonFlowStatus person
  _ <- QPS.create createPersonStats
  pure person
  where
    makeIdlePersonFlowStatus person =
      DPFS.PersonFlowStatus
        { personId = person.id,
          flowStatus = DPFS.IDLE,
          updatedAt = person.updatedAt
        }
    makePersonStats :: MonadTime m => SP.Person -> m DPS.PersonStats
    makePersonStats person = do
      now <- getCurrentTime
      return
        DPS.PersonStats
          { personId = person.id,
            userCancelledRides = 0,
            driverCancelledRides = 0,
            completedRides = 0,
            weekendRides = 0,
            weekdayRides = 0,
            offPeakRides = 0,
            eveningPeakRides = 0,
            morningPeakRides = 0,
            weekendPeakRides = 0,
            updatedAt = now
          }

checkPersonExists :: (CacheFlow m r, EsqDBFlow m r) => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM (PersonDoesNotExist entityId)

resend ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  Id SR.RegistrationToken ->
  m ResendAuthRes
resend tokenId = do
  SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  let merchantOperatingCityId = person.merchantOperatingCityId
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let otpCode = authValueHash
  let otpHash = smsCfg.credConfig.otpHash
      phoneNumber = countryCode <> mobileNumber
      sender = smsCfg.sender
  otpChannel <- getPersonOTPChannel person.id
  case otpChannel of
    SMS -> do
      withLogTag ("personId_" <> getId person.id) $ do
        message <-
          MessageBuilder.buildSendOTPMessage merchantOperatingCityId $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        Sms.sendSMS person.merchantId merchantOperatingCityId (Sms.SendSMSReq message phoneNumber sender)
          >>= Sms.checkSmsResult
    WHATSAPP ->
      withLogTag ("personId_" <> getId person.id) $ do
        result <- Whatsapp.whatsAppOtpApi person.merchantId merchantOperatingCityId (Whatsapp.SendOtpApiReq phoneNumber otpCode)
        when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp OTP message")

  void $ RegistrationToken.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1) authType Nothing Nothing (Just person.blocked)

cleanCachedTokens :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- RegistrationToken.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

logout ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id SP.Person ->
  m APISuccess
logout personId = do
  cleanCachedTokens personId
  void $ Person.updateDeviceToken personId Nothing
  void $ RegistrationToken.deleteByPersonId personId
  pure AP.Success

authHitOTPChannel :: Id SP.Person -> Text
authHitOTPChannel personId = "BAP:Registration:auth" <> getId personId <> ":OtpChannel"

cachePersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> OTPChannel -> m ()
cachePersonOTPChannel personId otpChannel = do
  Hedis.setExp (authHitOTPChannel personId) otpChannel 1800 -- 30 min

getPersonOTPChannel :: (CacheFlow m r, MonadFlow m) => Id SP.Person -> m OTPChannel
getPersonOTPChannel personId = do
  Hedis.get (authHitOTPChannel personId) >>= \case
    Just a -> pure a
    Nothing -> do
      pure SMS -- default otpChannel is SMS (for resend)
