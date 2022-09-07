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

import Beckn.External.Encryption
import Beckn.External.FCM.Types (FCMRecipientToken)
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common as BC
import Beckn.Types.Id
import qualified Beckn.Types.Predicate as P
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Common
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.SlidingWindowLimiter
import Beckn.Utils.Validation
import Data.OpenApi hiding (info)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Organization as DO
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.DriverInformation as QD
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth (authTokenCacheKey)
import Tools.Error
import Tools.Metrics

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    organizationId :: Text
  }
  deriving (Generic, FromJSON, ToSchema)

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
    deviceToken :: FCMRecipientToken
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
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["otpSmsTemplate" ::: Text],
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  AuthReq ->
  m AuthRes
auth req = do
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  person <-
    QP.findByMobileNumber countryCode mobileNumber
      >>= maybe (createDriverWhihDetails req) return
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  token <- makeSession scfg entityId SR.USER (show <$> useFakeOtpM)
  Esq.runTransaction $ QR.create token
  whenNothing_ useFakeOtpM $ do
    otpSmsTemplate <- asks (.otpSmsTemplate)
    withLogTag ("personId_" <> getId person.id) $
      SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
        >>= SF.checkSmsResult
  let attempts = SR.attempts token
      authId = SR.id token
  return $ AuthRes {attempts, authId}

createDriverDetails :: Id SP.Person -> Esq.SqlDB ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            active = False,
            onRide = False,
            enabled = False,
            verified = False,
            createdAt = now,
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QD.create driverInfo
  where
    driverId = cast personId

makePerson :: EncFlow m r => AuthReq -> m SP.Person
makePerson req = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt req.mobileNumber
  let orgId = Just (Id req.organizationId :: Id DO.Organization)
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
        mobileNumber = Just encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        organizationId = orgId,
        isNew = True,
        registered = False,
        deviceToken = Nothing,
        language = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
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

createDriverWhihDetails :: (EncFlow m r, EsqDBFlow m r) => AuthReq -> m SP.Person
createDriverWhihDetails req = do
  person <- makePerson req
  DB.runTransaction $ do
    QP.create person
    createDriverDetails (person.id)

  pure person

verify ::
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
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
  let isNewPerson = person.isNew
  let deviceToken = Just req.deviceToken
  cleanCachedTokens person.id
  Esq.runTransaction $ do
    QR.deleteByPersonIdExceptNew person.id tokenId
    QR.setVerified tokenId
    QP.updateDeviceToken person.id deviceToken
    when isNewPerson $
      QP.setIsNewFalse person.id
  updPers <- QP.findById (Id entityId) >>= fromMaybeM (PersonNotFound entityId)
  decPerson <- decrypt updPers
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

checkRegistrationTokenExists :: (Esq.Transactionable m, MonadThrow m, Log m) => Id SR.RegistrationToken -> m SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

checkPersonExists :: (Esq.Transactionable m, MonadThrow m, Log m) => Text -> m SP.Person
checkPersonExists entityId =
  QP.findById (Id entityId) >>= fromMaybeM (PersonNotFound entityId)

resend ::
  ( HasFlowEnv m r ["otpSmsTemplate" ::: Text, "smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m
  ) =>
  Id SR.RegistrationToken ->
  m ResendAuthRes
resend tokenId = do
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  otpSmsTemplate <- asks (.otpSmsTemplate)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  withLogTag ("personId_" <> entityId) $
    SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
      >>= SF.checkSmsResult
  Esq.runTransaction $ QR.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1)

cleanCachedTokens :: EsqDBFlow m r => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- QR.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key

logout ::
  ( EsqDBFlow m r
  ) =>
  Id SP.Person ->
  m APISuccess
logout personId = do
  cleanCachedTokens personId
  uperson <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  Esq.runTransaction $ do
    QP.updateDeviceToken uperson.id Nothing
    QR.deleteByPersonId personId
    when (uperson.role == SP.DRIVER) $ QD.updateActivity (cast uperson.id) False
  pure Success
