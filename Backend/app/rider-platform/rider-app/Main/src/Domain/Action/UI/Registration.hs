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
    auth,
    verify,
    resend,
    logout,
  )
where

-- import Kernel.External.SMS

import Data.OpenApi (ToSchema)
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
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess as AP
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
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Person.PersonFlowStatus as QDFS
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Auth (authTokenCacheKey)
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify
import qualified Tools.SMS as Sms
import Tools.Whatsapp

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: ShortId Merchant --,
    -- otpChannel :: Maybe OTPChannel
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

validateAuthReq :: Validate AuthReq
validateAuthReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
    ]

data AuthRes = AuthRes
  { authId :: Id RegistrationToken,
    attempts :: Int
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
  forall m r.
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["otpSmsTemplate" ::: Text],
    HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  AuthReq ->
  Maybe Version ->
  Maybe Version ->
  m AuthRes
auth req mbBundleVersion mbClientVersion = do
  runRequestValidation validateAuthReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  -- otpChannel = fromMaybe defaultOTPChannel req.otpChannel
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantNotFound $ getShortId req.merchantId)
  mobileNumberHash <- getDbHash mobileNumber
  person <-
    Person.findByRoleAndMobileNumberAndMerchantId SP.USER countryCode mobileNumberHash merchant.id (Proxy @m)
      >>= maybe (createPerson req mbBundleVersion mbClientVersion merchant.id) return
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg

  token <- makeSession scfg entityId (show <$> useFakeOtpM)

  if person.enabled
    then do
      DB.runTransaction $ Person.updatePersonVersions @m person mbBundleVersion mbClientVersion
      DB.runTransaction (RegistrationToken.create @m token)
      whenNothing_ useFakeOtpM $ do
        otpSmsTemplate <- asks (.otpSmsTemplate)
        let otpCode = SR.authValueHash token
        let otpHash = smsCfg.credConfig.otpHash
            phoneNumber = countryCode <> mobileNumber
            sender = smsCfg.sender
        withLogTag ("personId_" <> getId person.id) $
          Sms.sendSMS person.merchantId (Sms.constructSendSMSReq otpCode otpHash otpSmsTemplate phoneNumber sender)
            >>= Sms.checkSmsResult
    else logInfo $ "Person " <> getId person.id <> " is not enabled. Skipping send OTP"

  let attempts = SR.attempts token
      authId = SR.id token
  return $ AuthRes {attempts, authId}

buildPerson :: EncFlow m r => AuthReq -> Maybe Version -> Maybe Version -> Id DMerchant.Merchant -> m SP.Person
buildPerson req bundleVersion clientVersion merchantId = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt req.mobileNumber
  return $
    SP.Person
      { id = pid,
        firstName = Nothing,
        middleName = Nothing,
        lastName = Nothing,
        role = SP.USER,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        unencryptedMobileNumber = Just req.mobileNumber,
        mobileNumber = Just encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        isNew = True,
        enabled = True,
        deviceToken = Nothing,
        description = Nothing,
        merchantId = merchantId,
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

verify ::
  forall m r.
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
  let isNewPerson = person.isNew
  let deviceToken = Just req.deviceToken
  cleanCachedTokens person.id
  DB.runTransaction $ do
    RegistrationToken.deleteByPersonIdExceptNew @m person.id tokenId
    RegistrationToken.setVerified tokenId
    Person.updateDeviceToken person.id deviceToken
    when isNewPerson $
      Person.setIsNewFalse person.id
  when isNewPerson $
    Notify.notifyOnRegistration regToken person deviceToken
  updPerson <- Person.findById (Id entityId) (Proxy @m) >>= fromMaybeM (PersonDoesNotExist entityId)
  decPerson <- decrypt updPerson
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  unless (decPerson.whatsappNotificationEnrollStatus == req.whatsappNotificationEnroll && isJust req.whatsappNotificationEnroll) $ do
    fork "whatsapp_opt_api_call" $ do
      case decPerson.mobileNumber of
        Nothing -> throwError $ AuthBlocked "Mobile Number is null"
        Just mobileNo -> callWhatsappOptApi mobileNo person.id person.merchantId req.whatsappNotificationEnroll
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

callWhatsappOptApi ::
  forall m r.
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
    Person.updateWhatsappNotificationEnrollStatus @m personId $ Just status

getRegistrationTokenE :: forall m r. EsqDBFlow m r => Id SR.RegistrationToken -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId (Proxy @m) >>= fromMaybeM (TokenNotFound $ getId tokenId)

createPerson :: forall m r. (EncFlow m r, EsqDBFlow m r) => AuthReq -> Maybe Version -> Maybe Version -> Id DMerchant.Merchant -> m SP.Person
createPerson req mbBundleVersion mbClientVersion merchantId = do
  person <- buildPerson req mbBundleVersion mbClientVersion merchantId
  DB.runTransaction $ do
    Person.create @m person
    QDFS.create $ makeIdlePersonFlowStatus person
  pure person
  where
    makeIdlePersonFlowStatus person =
      DPFS.PersonFlowStatus
        { personId = person.id,
          flowStatus = DPFS.IDLE,
          updatedAt = person.updatedAt
        }

checkPersonExists :: forall m r. EsqDBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) (Proxy @m) >>= fromMaybeM (PersonDoesNotExist entityId)

resend ::
  forall m r.
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["otpSmsTemplate" ::: Text],
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
  otpSmsTemplate <- asks (.otpSmsTemplate)
  smsCfg <- asks (.smsCfg)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let otpCode = authValueHash
  let otpHash = smsCfg.credConfig.otpHash
      phoneNumber = countryCode <> mobileNumber
      sender = smsCfg.sender
  withLogTag ("personId_" <> entityId) $
    Sms.sendSMS person.merchantId (Sms.constructSendSMSReq otpCode otpHash otpSmsTemplate phoneNumber sender)
      >>= Sms.checkSmsResult
  DB.runTransaction $ RegistrationToken.updateAttempts @m (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1)

cleanCachedTokens :: forall m r. (EsqDBFlow m r, Redis.HedisFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- RegistrationToken.findAllByPersonId personId (Proxy @m)
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

logout ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id SP.Person ->
  m APISuccess
logout personId = do
  cleanCachedTokens personId
  DB.runTransaction $ do
    Person.updateDeviceToken @m personId Nothing
    RegistrationToken.deleteByPersonId personId
  pure AP.Success
