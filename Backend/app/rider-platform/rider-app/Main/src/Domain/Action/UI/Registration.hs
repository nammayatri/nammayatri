{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Registration
  ( AuthRawReq (..),
    AuthReq (..),
    AuthRes (..),
    ResendAuthRes,
    AuthVerifyReq (..),
    AuthVerifyRes (..),
    auth,
    verify,
    resend,
    logout,
  )
where

-- import Kernel.External.SMS

import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.Store.X509 as CryptoStore
import qualified Data.Aeson as A
import Data.Aeson.Types ((.:), (.:?))
import qualified Data.ByteString.Base64 as B64
import Data.OpenApi hiding (email, info)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.X509 (PubKey (PubKeyRSA))
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person (PersonAPIEntity, PersonE (updatedAt))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.RegistrationToken (RegistrationToken)
import qualified Domain.Types.RegistrationToken as SR
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Maps as Maps
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess as AP
import Kernel.Types.Base64
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
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Merchant.MerchantConfig as MerchantConfig
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Person.PersonFlowStatus as QDFS
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Auth (authTokenCacheKey)
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify
import qualified Tools.SMS as Sms
import Tools.Whatsapp

newtype AuthRawReq = AuthRawReq ByteString deriving (Generic, Show)

instance ToSchema AuthRawReq where
  declareNamedSchema _ = lift $ Identity (NamedSchema Nothing mempty)

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: ShortId Merchant,
    deviceToken :: Maybe FCMRecipientToken,
    whatsappNotificationEnroll :: Maybe Whatsapp.OptApiMethods,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe SP.Gender,
    timestamp :: Maybe UTCTime
    -- otpChannel :: Maybe OTPChannel
  }
  deriving (Generic, ToJSON, Show, ToSchema)

instance A.FromJSON AuthReq where
  parseJSON v = case v of
    A.Object obj ->
      AuthReq <$> obj .: "mobileNumber"
        <*> obj .: "mobileCountryCode"
        <*> obj .: "merchantId"
        <*> obj .:? "deviceToken"
        <*> obj .:? "whatsappNotificationEnroll"
        <*> obj .:? "firstName"
        <*> obj .:? "middleName"
        <*> obj .:? "lastName"
        <*> obj .:? "email"
        <*> obj .:? "language"
        <*> obj .:? "gender"
        <*> obj .:? "timestamp"
    A.String s ->
      case A.eitherDecodeStrict (TE.encodeUtf8 s) of
        Left err -> fail err
        Right req -> return req
    _ -> fail "Invalid JSON format for AuthReq"

validateAuthReq :: Validate AuthReq
validateAuthReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
    ]

data AuthRes = AuthRes
  { authId :: Id RegistrationToken,
    attempts :: Int,
    authType :: SR.LoginType,
    token :: Maybe Text,
    person :: Maybe PersonAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

-- Need to have discussion around this
-- data OTPChannel = SMS | WHATSAPP
--   deriving (Generic, Show, Enum, Eq, FromJSON, ToJSON, ToSchema)

-- defaultOTPChannel :: OTPChannel
-- defaultOTPChannel = SMS

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
    HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  AuthRawReq ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  m AuthRes
auth (AuthRawReq rawReq) mbSignature mbBundleVersion mbClientVersion = do
  req :: AuthReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  runRequestValidation validateAuthReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
      deviceToken = req.deviceToken
  -- otpChannel = fromMaybe defaultOTPChannel req.otpChannel
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantNotFound $ getShortId req.merchantId)
  mobileNumberHash <- getDbHash mobileNumber
  person <-
    Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash merchant.id
      >>= maybe (createPerson mobileNumber countryCode deviceToken mbBundleVersion mbClientVersion merchant.id req.email req.firstName req.middleName req.lastName req.language req.gender) return
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg

  regToken <- makeSession scfg entityId (show <$> useFakeOtpM)

  if person.enabled
    then do
      DB.runTransaction $ Person.updatePersonVersions person mbBundleVersion mbClientVersion
      DB.runTransaction (RegistrationToken.create regToken)
      when (isNothing useFakeOtpM && isNothing mbSignature) $ do
        let otpCode = SR.authValueHash regToken
        let otpHash = smsCfg.credConfig.otpHash
            phoneNumber = countryCode <> mobileNumber
            sender = smsCfg.sender
        withLogTag ("personId_" <> getId person.id) $ do
          message <-
            MessageBuilder.buildSendOTPMessage merchant.id $
              MessageBuilder.BuildSendOTPMessageReq
                { otp = otpCode,
                  hash = otpHash
                }
          Sms.sendSMS person.merchantId (Sms.SendSMSReq message phoneNumber sender)
            >>= Sms.checkSmsResult
    else logInfo $ "Person " <> getId person.id <> " is not enabled. Skipping send OTP"

  (personApiEntity, token, authType) <-
    if person.enabled
      then case (mbSignature, req.timestamp) of
        (Just signature, Just timestamp) -> do
          merchantConfig <- MerchantConfig.findById merchant.id >>= fromMaybeM (InternalError $ "Merchant config not found for " <> merchant.id.getId)
          let nominal = realToFrac merchantConfig.signatureExpiry
          expired <- isExpired nominal timestamp
          let decodedSignature = B64.decodeLenient . TE.encodeUtf8 $ signature
              publicKeys = getRSAPublicKey merchantConfig.signingPublicKey
          case publicKeys of
            [PubKeyRSA publicKey] -> do
              let isVerified = RSA.verify (Just Hash.SHA256) publicKey rawReq decodedSignature
              if isVerified && not expired
                then do
                  DB.runTransaction $ RegistrationToken.setDirectAuth regToken.id
                  personAPIEntity <- verifyFlow person regToken req.whatsappNotificationEnroll deviceToken
                  return (Just personAPIEntity, Just regToken.token, SR.DIRECT)
                else return (Nothing, Nothing, regToken.authType)
            _ -> return (Nothing, Nothing, regToken.authType)
        _ -> return (Nothing, Nothing, regToken.authType)
      else return (Nothing, Nothing, regToken.authType)
  return $ AuthRes regToken.id regToken.attempts authType token personApiEntity
  where
    getRSAPublicKey (Base64 key) = CryptoStore.readPubKeyFileFromMemory key

buildPerson ::
  EncFlow m r =>
  Text ->
  Text ->
  Maybe FCMRecipientToken ->
  Maybe Version ->
  Maybe Version ->
  Id DMerchant.Merchant ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Maps.Language ->
  Maybe SP.Gender ->
  m SP.Person
buildPerson mobileNumber countryCode deviceToken bundleVersion clientVersion merchantId mbEmail mbFirstName mbMiddleName mbLastName mbLanguage mbGender = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt mobileNumber
  encEmail <- mapM encrypt mbEmail
  return $
    SP.Person
      { id = pid,
        firstName = mbFirstName,
        middleName = mbMiddleName,
        lastName = mbLastName,
        role = SP.USER,
        gender = fromMaybe SP.UNKNOWN mbGender,
        identifierType = SP.MOBILENUMBER,
        email = encEmail,
        passwordHash = Nothing,
        unencryptedMobileNumber = Just mobileNumber,
        mobileNumber = Just encMobNum,
        mobileCountryCode = Just countryCode,
        identifier = Nothing,
        rating = Nothing,
        language = mbLanguage,
        isNew = True,
        enabled = True,
        blocked = False,
        deviceToken = deviceToken,
        description = Nothing,
        merchantId = merchantId,
        referralCode = Nothing,
        referredAt = Nothing,
        hasTakenValidRide = False,
        createdAt = now,
        updatedAt = now,
        bundleVersion = bundleVersion,
        clientVersion = clientVersion,
        whatsappNotificationEnrollStatus = Nothing
      }

-- FIXME Why do we need to store always the same authExpiry and tokenExpiry from config? info field is always Nothing
makeSession ::
  MonadFlow m =>
  SmsSessionConfig ->
  Text ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId fakeOtp = do
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
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BAP:Registration:verify:" <> getId id <> ":hitsCount"

verifyFlow :: (EsqDBFlow m r, EncFlow m r, CoreMetrics m, CacheFlow m r) => SP.Person -> SR.RegistrationToken -> Maybe Whatsapp.OptApiMethods -> Maybe FCMRecipientToken -> m PersonAPIEntity
verifyFlow person regToken whatsappNotificationEnroll deviceToken = do
  let isNewPerson = person.isNew
  DB.runTransaction $ do
    RegistrationToken.setDirectAuth regToken.id
    RegistrationToken.deleteByPersonIdExceptNew person.id regToken.id
    when isNewPerson $
      Person.setIsNewFalse person.id
  when isNewPerson $
    Notify.notifyOnRegistration regToken person deviceToken
  updPerson <- Person.findById (Id regToken.entityId) >>= fromMaybeM (PersonDoesNotExist regToken.entityId)
  decPerson <- decrypt updPerson
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  unless (decPerson.whatsappNotificationEnrollStatus == whatsappNotificationEnroll && isJust whatsappNotificationEnroll) $ do
    fork "whatsapp_opt_api_call" $ do
      case decPerson.mobileNumber of
        Nothing -> throwError $ AuthBlocked "Mobile Number is null"
        Just mobileNo -> callWhatsappOptApi mobileNo person.id person.merchantId whatsappNotificationEnroll
  return personAPIEntity

verify ::
  ( HasCacheConfig r,
    HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics m,
    CacheFlow m r
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
  let deviceToken = Just req.deviceToken
  cleanCachedTokens person.id
  DB.runTransaction $ RegistrationToken.setVerified tokenId
  personAPIEntity <- verifyFlow person regToken req.whatsappNotificationEnroll deviceToken
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

callWhatsappOptApi ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    CoreMetrics m,
    EncFlow m r,
    CacheFlow m r
  ) =>
  Text ->
  Id SP.Person ->
  Id DMerchant.Merchant ->
  Maybe Whatsapp.OptApiMethods ->
  m ()
callWhatsappOptApi mobileNo personId merchantId hasOptedIn = do
  let status = fromMaybe Whatsapp.OPT_IN hasOptedIn
  void $ whatsAppOptAPI merchantId $ Whatsapp.OptApiReq {phoneNumber = mobileNo, method = status}
  DB.runTransaction $
    Person.updateWhatsappNotificationEnrollStatus personId $ Just status

getRegistrationTokenE :: EsqDBFlow m r => Id SR.RegistrationToken -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

createPerson ::
  (EncFlow m r, EsqDBFlow m r) =>
  Text ->
  Text ->
  Maybe FCMRecipientToken ->
  Maybe Version ->
  Maybe Version ->
  Id DMerchant.Merchant ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Maps.Language ->
  Maybe SP.Gender ->
  m SP.Person
createPerson mobileNumber countryCode deviceToken mbBundleVersion mbClientVersion merchantId mbEmail mbFirstName mbMiddleName mbLastName mbLanguage mbGender = do
  person <- buildPerson mobileNumber countryCode deviceToken mbBundleVersion mbClientVersion merchantId mbEmail mbFirstName mbMiddleName mbLastName mbLanguage mbGender
  DB.runTransaction $ do
    Person.create person
    QDFS.create $ makeIdlePersonFlowStatus person
  pure person
  where
    makeIdlePersonFlowStatus person =
      DPFS.PersonFlowStatus
        { personId = person.id,
          flowStatus = DPFS.IDLE,
          updatedAt = person.updatedAt
        }

checkPersonExists :: EsqDBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM (PersonDoesNotExist entityId)

resend ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    CoreMetrics m
  ) =>
  Id SR.RegistrationToken ->
  m ResendAuthRes
resend tokenId = do
  SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
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
      MessageBuilder.buildSendOTPMessage person.merchantId $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otpCode,
            hash = otpHash
          }
    Sms.sendSMS person.merchantId (Sms.SendSMSReq message phoneNumber sender)
      >>= Sms.checkSmsResult
  DB.runTransaction $ RegistrationToken.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1) authType Nothing Nothing

cleanCachedTokens :: (EsqDBFlow m r, Redis.HedisFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- RegistrationToken.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

logout ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id SP.Person ->
  m APISuccess
logout personId = do
  cleanCachedTokens personId
  DB.runTransaction $ do
    Person.updateDeviceToken personId Nothing
    RegistrationToken.deleteByPersonId personId
  pure AP.Success
