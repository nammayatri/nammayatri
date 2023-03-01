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

import Data.OpenApi hiding (info, url)
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Merchant as DO
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.External.FCM.Types (FCMRecipientToken)
import Kernel.External.Whatsapp.Interface.Types as Whatsapp
import Kernel.Sms.Config
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Common as BC
import Kernel.Types.Id
import qualified Kernel.Types.Predicate as P
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as QD
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth (authTokenCacheKey)
import Tools.Error
import Tools.Metrics
import Tools.SMS as Sms hiding (Success)
import Tools.Whatsapp as Whatsapp

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: Text
  }
  deriving (Generic, FromJSON, ToSchema)

validateInitiateLoginReq :: Validate AuthReq
validateInitiateLoginReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
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
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  mobileNumberHash <- getDbHash mobileNumber
  let merchantId = Id req.merchantId :: Id DO.Merchant
  merchant <-
    QMerchant.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  person <-
    QP.findByMobileNumber countryCode mobileNumberHash (Proxy @m)
      >>= maybe (createDriverWithDetails req mbBundleVersion mbClientVersion merchant.id) return
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  token <- makeSession scfg entityId SR.USER (show <$> useFakeOtpM)
  Esq.runTransaction do
    QR.create @m token
    QP.updatePersonVersions person mbBundleVersion mbClientVersion
  whenNothing_ useFakeOtpM $ do
    otpSmsTemplate <- asks (.otpSmsTemplate)
    let otpHash = smsCfg.credConfig.otpHash
    let otpCode = SR.authValueHash token
        phoneNumber = countryCode <> mobileNumber
        sender = smsCfg.sender
    withLogTag ("personId_" <> getId person.id) $
      Sms.sendSMS person.merchantId (Sms.constructSendSMSReq otpCode otpHash otpSmsTemplate phoneNumber sender)
        >>= Sms.checkSmsResult
  let attempts = SR.attempts token
      authId = SR.id token
  return $ AuthRes {attempts, authId}

createDriverDetails :: Id SP.Person -> Esq.SqlDB m ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            adminId = Nothing,
            active = False,
            onRide = False,
            enabled = False,
            blocked = False,
            verified = False,
            referralCode = Nothing,
            lastEnabledOn = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QD.create driverInfo
  where
    driverId = cast personId

makePerson :: EncFlow m r => AuthReq -> Maybe Version -> Maybe Version -> Id DO.Merchant -> m SP.Person
makePerson req mbBundleVersion mbClientVersion merchantId = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt req.mobileNumber
  return $
    SP.Person
      { id = pid,
        firstName = "Driver",
        middleName = Nothing,
        lastName = Nothing,
        role = SP.DRIVER,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        unencryptedMobileNumber = Just req.mobileNumber,
        mobileNumber = Just encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        merchantId = merchantId,
        isNew = True,
        deviceToken = Nothing,
        language = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now,
        bundleVersion = mbBundleVersion,
        clientVersion = mbClientVersion,
        whatsappNotificationEnrollStatus = Nothing
      }

makeSession ::
  MonadFlow m =>
  SmsSessionConfig ->
  Text ->
  SR.RTEntityType ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId entityType fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- generateGUID
  token <- generateGUID
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
        entityType = entityType,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BPP:Registration:verify:" <> getId id <> ":hitsCount"

createDriverWithDetails :: forall m r. (EncFlow m r, EsqDBFlow m r) => AuthReq -> Maybe Version -> Maybe Version -> Id DO.Merchant -> m SP.Person
createDriverWithDetails req mbBundleVersion mbClientVersion mercahntId = do
  person <- makePerson req mbBundleVersion mbClientVersion mercahntId
  DB.runTransaction $ do
    QP.create @m person
    QDFS.create $ makeIdleDriverFlowStatus person
    createDriverDetails (person.id)
  pure person
  where
    makeIdleDriverFlowStatus person =
      DDFS.DriverFlowStatus
        { personId = person.id,
          flowStatus = DDFS.IDLE,
          updatedAt = person.updatedAt
        }

verify ::
  forall m r.
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
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
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId (Proxy @m)
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId (Proxy @m)
  let isNewPerson = person.isNew
  let deviceToken = Just req.deviceToken
  cleanCachedTokens person.id
  Esq.runTransaction $ do
    QR.deleteByPersonIdExceptNew @m person.id tokenId
    QR.setVerified tokenId
    QP.updateDeviceToken person.id deviceToken
    when isNewPerson $
      QP.setIsNewFalse person.id
  updPers <- QP.findById (Proxy @m) (Id entityId) >>= fromMaybeM (PersonNotFound entityId)
  decPerson <- decrypt updPers
  unless (decPerson.whatsappNotificationEnrollStatus == req.whatsappNotificationEnroll && isJust req.whatsappNotificationEnroll) $ do
    fork "whatsapp_opt_api_call" $ do
      case decPerson.mobileNumber of
        Nothing -> throwError $ AuthBlocked "Mobile Number is null"
        Just mobileNo -> callWhatsappOptApi mobileNo person.merchantId person.id req.whatsappNotificationEnroll
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

callWhatsappOptApi ::
  forall m r.
  ( EsqDBFlow m r,
    CoreMetrics m,
    EncFlow m r,
    CacheFlow m r
  ) =>
  Text ->
  Id DO.Merchant ->
  Id SP.Person ->
  Maybe Whatsapp.OptApiMethods ->
  m ()
callWhatsappOptApi mobileNo merchantId personId hasOptedIn = do
  let status = fromMaybe Whatsapp.OPT_IN hasOptedIn
  void $ Whatsapp.whatsAppOptAPI merchantId $ Whatsapp.OptApiReq {phoneNumber = mobileNo, method = status}
  DB.runTransaction $
    QP.updateWhatsappNotificationEnrollStatus @m personId $ Just status

checkRegistrationTokenExists :: forall m ma. (Esq.Transactionable ma m, MonadThrow m, Log m) => Id SR.RegistrationToken -> Proxy ma -> m SR.RegistrationToken
checkRegistrationTokenExists tokenId proxy =
  QR.findById proxy tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

checkPersonExists :: forall m ma. (Esq.Transactionable ma m, MonadThrow m, Log m) => Text -> Proxy ma -> m SP.Person
checkPersonExists entityId proxy =
  QP.findById proxy (Id entityId) >>= fromMaybeM (PersonNotFound entityId)

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
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId (Proxy @m)
  person <- checkPersonExists entityId (Proxy @m)
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  otpSmsTemplate <- asks (.otpSmsTemplate)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let otpCode = authValueHash
  let otpHash = smsCfg.credConfig.otpHash
      phoneNumber = countryCode <> mobileNumber
      sender = smsCfg.sender
  withLogTag ("personId_" <> entityId) $
    Sms.sendSMS person.merchantId (Sms.constructSendSMSReq otpCode otpHash otpSmsTemplate phoneNumber sender)
      >>= Sms.checkSmsResult
  Esq.runTransaction $ QR.updateAttempts @m (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1)

cleanCachedTokens :: forall m r. (EsqDBFlow m r, Redis.HedisFlow m r) => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId (Proxy @m)
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.del key

logout ::
  forall m r.
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Id SP.Person ->
  m APISuccess
logout personId = do
  cleanCachedTokens personId
  uperson <-
    QP.findById (Proxy @m) personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  Esq.runTransaction $ do
    QP.updateDeviceToken @m uperson.id Nothing
    QR.deleteByPersonId personId
  when (uperson.role == SP.DRIVER) $ QD.updateActivity (cast uperson.id) False
  pure Success
